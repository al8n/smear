#!/usr/bin/env python3
"""Fail-closed controller for coordinated smear crate releases.

The workflow around this file deliberately separates three authorities:

* ``prepare`` is read-only. It creates the ignored workspace lock and records
  the exact package archives produced from it.
* ``publish`` runs behind the ``crates-io`` GitHub environment. It reconstructs
  that lock, proves the archives are byte-identical, obtains one OIDC token in
  the workflow, and publishes missing versions in dependency order.
* ``release`` has GitHub contents permission but no OIDC token. It packages a
  third time, verifies the public registry, then creates the immutable tag and
  GitHub Release without moving or replacing anything that already exists.

All registry decisions are fail-closed. A version that already exists is
accepted only with the exact archive checksum and provenance described by
``classify_version`` below.
"""

from __future__ import annotations

import argparse
import base64
from collections.abc import Callable
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys
import tarfile
import time
import unittest
import urllib.error
import urllib.parse
import urllib.request


REPOSITORY = "al8n/smear"
DEFAULT_BRANCH = "main"
TOOLCHAIN = "1.99.1"
WORKFLOW_FILENAME = "release.yml"
TRUSTED_ENVIRONMENT = "crates-io"
USER_AGENT = f"smear-release/{TOOLCHAIN} (+https://github.com/{REPOSITORY})"

PUBLISH_ORDER = (
    "smear-lexer",
    "smear-parser",
    "smear-schema",
    "smear-compiler",
    "graphql-proto",
    "smear",
)
BOOTSTRAP_0_0_1 = frozenset(PUBLISH_ORDER[:-1])
UNPUBLISHED_MEMBERS = frozenset(("smear-noatomic", "smear-smoke", "source-census"))
INTERNAL_EDGES = {
    "smear-lexer": frozenset(),
    "smear-parser": frozenset(("smear-lexer",)),
    "smear-schema": frozenset(("smear-parser",)),
    "smear-compiler": frozenset(("smear-parser", "smear-schema")),
    "graphql-proto": frozenset(("smear-parser", "smear-schema")),
    "smear": frozenset(
        ("graphql-proto", "smear-compiler", "smear-lexer", "smear-parser", "smear-schema")
    ),
}
REQUIRED_WORKFLOWS = {
    "CI": ".github/workflows/ci.yml",
    "macOS": ".github/workflows/macos.yml",
    "Miri": ".github/workflows/miri.yml",
}

VERSION_RE = re.compile(r"^[0-9]+\.[0-9]+\.[0-9]+$")
COMMIT_RE = re.compile(r"^[0-9a-f]{40}$")
SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
MAX_GITHUB_OUTPUT_UTF16_BYTES = 900_000


class ReleaseError(RuntimeError):
    """A release invariant failed."""


class HttpStatusError(ReleaseError):
    def __init__(self, status: int, url: str, detail: str = "") -> None:
        suffix = f": {detail}" if detail else ""
        super().__init__(f"HTTP {status} from {url}{suffix}")
        self.status = status
        self.url = url
        self.detail = detail


def fail(message: str) -> None:
    raise ReleaseError(message)


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def canonical_json_bytes(value: object) -> bytes:
    return (json.dumps(value, sort_keys=True, separators=(",", ":")) + "\n").encode()


def load_json(path: Path) -> object:
    with path.open("rb") as handle:
        return json.load(handle)


def write_json(path: Path, value: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(canonical_json_bytes(value))


def run(
    argv: list[str],
    *,
    capture: bool = False,
    check: bool = True,
    env: dict[str, str] | None = None,
) -> subprocess.CompletedProcess[str]:
    print(f"+ {shlex.join(argv)}", flush=True)
    result = subprocess.run(
        argv,
        check=False,
        text=True,
        stdout=subprocess.PIPE if capture else None,
        stderr=subprocess.PIPE if capture else None,
        env=env,
    )
    if check and result.returncode != 0:
        detail = ""
        if capture:
            detail = f"\nstdout:\n{result.stdout}\nstderr:\n{result.stderr}"
        fail(f"command exited {result.returncode}: {shlex.join(argv)}{detail}")
    return result


def cargo_args(*args: str) -> list[str]:
    return ["cargo", f"+{TOOLCHAIN}", *args]


def cargo_env(target_dir: Path, *, allow_token: bool = False) -> dict[str, str]:
    env = dict(os.environ)
    env["CARGO_TARGET_DIR"] = str(target_dir)
    if not allow_token:
        env.pop("CARGO_REGISTRY_TOKEN", None)
        env.pop("CARGO_REGISTRIES_CRATES_IO_TOKEN", None)
    return env


class HttpClient:
    def __init__(self, token: str | None = None) -> None:
        self.token = token

    def request_bytes(
        self,
        method: str,
        url: str,
        *,
        body: bytes | None = None,
        content_type: str | None = None,
        accept: str = "application/json",
    ) -> bytes:
        headers = {
            "Accept": accept,
            "User-Agent": USER_AGENT,
            "X-GitHub-Api-Version": "2022-11-28",
        }
        if self.token:
            headers["Authorization"] = f"Bearer {self.token}"
        if content_type:
            headers["Content-Type"] = content_type
        request = urllib.request.Request(url, data=body, headers=headers, method=method)
        try:
            with urllib.request.urlopen(request, timeout=30) as response:
                return response.read()
        except urllib.error.HTTPError as error:
            detail = error.read().decode(errors="replace")[:1000]
            raise HttpStatusError(error.code, url, detail) from error
        except urllib.error.URLError as error:
            raise ReleaseError(f"request failed for {url}: {error.reason}") from error

    def request_json(
        self,
        method: str,
        url: str,
        *,
        value: object | None = None,
    ) -> object:
        body = canonical_json_bytes(value) if value is not None else None
        data = self.request_bytes(
            method,
            url,
            body=body,
            content_type="application/json" if body is not None else None,
        )
        try:
            return json.loads(data)
        except json.JSONDecodeError as error:
            raise ReleaseError(f"non-JSON response from {url}") from error

    def request_json_bytes(self, url: str, data: bytes) -> object:
        response = self.request_bytes(
            "POST",
            url,
            body=data,
            content_type="application/octet-stream",
            accept="application/vnd.github+json",
        )
        try:
            return json.loads(response)
        except json.JSONDecodeError as error:
            raise ReleaseError(f"non-JSON asset-upload response from {url}") from error


def github_api_url(path: str) -> str:
    return f"{os.environ.get('GITHUB_API_URL', 'https://api.github.com').rstrip('/')}/{path.lstrip('/')}"


def validate_version_commit(version: str, commit: str) -> None:
    if not VERSION_RE.fullmatch(version):
        fail(f"release version must be MAJOR.MINOR.PATCH, got {version!r}")
    if not COMMIT_RE.fullmatch(commit):
        fail(f"release commit must be a full lowercase 40-character SHA, got {commit!r}")


def verify_event_context(version: str, commit: str) -> None:
    validate_version_commit(version, commit)
    repository = os.environ.get("GITHUB_REPOSITORY")
    github_ref = os.environ.get("GITHUB_REF")
    github_sha = os.environ.get("GITHUB_SHA")
    if repository is not None and repository != REPOSITORY:
        fail(f"workflow repository is {repository!r}, expected {REPOSITORY!r}")
    if github_ref is not None and github_ref != f"refs/heads/{DEFAULT_BRANCH}":
        fail(f"release must be dispatched from refs/heads/{DEFAULT_BRANCH}, got {github_ref!r}")
    if github_sha is not None and github_sha != commit:
        fail(f"workflow event SHA {github_sha} does not equal requested commit {commit}")


def verify_git_commit(commit: str) -> None:
    head = run(["git", "rev-parse", "HEAD"], capture=True).stdout.strip()
    if head != commit:
        fail(f"checked-out HEAD {head} does not equal release commit {commit}")
    status = run(
        ["git", "status", "--porcelain", "--untracked-files=all"], capture=True
    ).stdout.strip()
    if status:
        fail(f"release checkout is not clean:\n{status}")


def verify_github_gates(client: HttpClient, commit: str) -> None:
    query = urllib.parse.urlencode({"head_sha": commit, "event": "push", "per_page": "100"})
    url = github_api_url(f"repos/{REPOSITORY}/actions/runs?{query}")
    payload = client.request_json("GET", url)
    if not isinstance(payload, dict) or not isinstance(payload.get("workflow_runs"), list):
        fail("GitHub Actions response did not contain workflow_runs")
    runs = payload["workflow_runs"]
    for name, path in REQUIRED_WORKFLOWS.items():
        candidates = [
            item
            for item in runs
            if item.get("path") == path
            and item.get("head_sha") == commit
            and item.get("event") == "push"
        ]
        if not candidates:
            fail(f"no push run of {name} ({path}) exists for {commit}")
        latest = max(candidates, key=lambda item: int(item.get("id", 0)))
        status = latest.get("status")
        conclusion = latest.get("conclusion")
        if status != "completed" or conclusion != "success":
            fail(
                f"{name} gate for {commit} is {status}/{conclusion}, expected completed/success"
            )
        print(f"ok: {name} gate run {latest.get('id')} completed successfully")


def cargo_metadata(target_dir: Path) -> dict[str, object]:
    result = run(
        cargo_args("metadata", "--locked", "--no-deps", "--format-version", "1"),
        capture=True,
        env=cargo_env(target_dir),
    )
    try:
        value = json.loads(result.stdout)
    except json.JSONDecodeError as error:
        fail(f"cargo metadata returned invalid JSON: {error}")
    if not isinstance(value, dict):
        fail("cargo metadata did not return an object")
    return value


def verify_workspace(metadata: dict[str, object], version: str) -> dict[str, dict[str, object]]:
    workflow = Path(".github/workflows") / WORKFLOW_FILENAME
    if not workflow.is_file():
        fail(f"trusted-publishing workflow is missing: {workflow}")
    packages = metadata.get("packages")
    workspace_members = set(metadata.get("workspace_members", []))
    if not isinstance(packages, list):
        fail("cargo metadata did not contain a package list")
    by_name = {
        package["name"]: package
        for package in packages
        if package.get("id") in workspace_members
    }
    publishable = {
        name for name, package in by_name.items() if package.get("publish") != []
    }
    if publishable != set(PUBLISH_ORDER):
        fail(
            "publishable workspace members differ: "
            f"got {sorted(publishable)}, expected {list(PUBLISH_ORDER)}"
        )
    missing_unpublished = [
        name
        for name in sorted(UNPUBLISHED_MEMBERS)
        if name not in by_name or by_name[name].get("publish") != []
    ]
    if missing_unpublished:
        fail(f"workspace checks are not publish=false: {missing_unpublished}")
    expected_req = f"^{version}"
    for name in PUBLISH_ORDER:
        package = by_name[name]
        if package.get("version") != version:
            fail(f"{name} is version {package.get('version')}, expected {version}")
        actual_edges: set[str] = set()
        for dependency in package.get("dependencies", []):
            if dependency.get("kind") is not None or dependency.get("path") is None:
                continue
            dependency_name = dependency.get("name")
            if dependency_name in PUBLISH_ORDER:
                actual_edges.add(dependency_name)
                if dependency.get("req") != expected_req:
                    fail(
                        f"{name} requires {dependency_name} {dependency.get('req')}, "
                        f"expected {expected_req}"
                    )
        if actual_edges != set(INTERNAL_EDGES[name]):
            fail(
                f"{name} internal edges are {sorted(actual_edges)}, "
                f"expected {sorted(INTERNAL_EDGES[name])}"
            )
    print(f"ok: exact six-package {version} release graph verified")
    return by_name


def extract_package_for_manifest(
    bundle: tarfile.TarFile, target_dir: Path, root: str
) -> Path:
    extraction_root = target_dir / "package-manifests"
    package_root = extraction_root / root
    if package_root.exists():
        shutil.rmtree(package_root)
    for member in bundle.getmembers():
        relative = Path(member.name)
        if relative.is_absolute() or not relative.parts or relative.parts[0] != root:
            fail(f"package archive contains an out-of-root path: {member.name!r}")
        if any(part in ("", ".", "..") for part in relative.parts):
            fail(f"package archive contains an unsafe path: {member.name!r}")
        destination = extraction_root.joinpath(*relative.parts)
        if member.isdir():
            destination.mkdir(parents=True, exist_ok=True)
            continue
        if not member.isfile():
            fail(f"package archive contains a non-file entry: {member.name!r}")
        source = bundle.extractfile(member)
        if source is None:
            fail(f"failed to read package archive entry {member.name!r}")
        destination.parent.mkdir(parents=True, exist_ok=True)
        with destination.open("wb") as output:
            shutil.copyfileobj(source, output)
    manifest_path = package_root / "Cargo.toml"
    if not manifest_path.is_file():
        fail(f"extracted package has no normalized manifest: {manifest_path}")
    return manifest_path


def read_packaged_manifest(manifest_path: Path, target_dir: Path) -> dict[str, object]:
    result = run(
        cargo_args("read-manifest", "--manifest-path", str(manifest_path)),
        capture=True,
        env=cargo_env(target_dir),
    )
    try:
        manifest = json.loads(result.stdout)
    except json.JSONDecodeError as error:
        fail(f"cargo read-manifest returned invalid JSON for {manifest_path}: {error}")
    if not isinstance(manifest, dict):
        fail(f"cargo read-manifest returned a non-object for {manifest_path}")
    return manifest


def validate_packaged_manifest(
    manifest: dict[str, object], name: str, version: str
) -> None:
    if manifest.get("name") != name:
        fail(f"packaged manifest names {manifest.get('name')!r}, expected {name!r}")
    if manifest.get("version") != version:
        fail(f"{name}'s packaged manifest has version {manifest.get('version')!r}, expected {version}")
    dependencies = manifest.get("dependencies")
    if not isinstance(dependencies, list):
        fail(f"{name}'s packaged manifest has no dependency list")
    normal_internal: dict[str, dict[str, object]] = {}
    for dependency in dependencies:
        if not isinstance(dependency, dict) or not isinstance(dependency.get("name"), str):
            fail(f"{name}'s packaged manifest contains invalid dependency metadata")
        dependency_name = dependency["name"]
        if dependency_name in PUBLISH_ORDER and dependency.get("kind") is None:
            if dependency_name in normal_internal:
                fail(f"{name}'s packaged manifest repeats dependency {dependency_name}")
            normal_internal[dependency_name] = dependency
        if name == "smear" and dependency_name == "smear" and dependency.get("kind") == "dev":
            fail(f"{name}'s packaged manifest retained its path-only self dev-dependency")
    if set(normal_internal) != set(INTERNAL_EDGES[name]):
        fail(
            f"{name}'s packaged manifest has internal edges {sorted(normal_internal)}, "
            f"expected {sorted(INTERNAL_EDGES[name])}"
        )
    for dependency_name, dependency in normal_internal.items():
        if dependency.get("req") != f"^{version}":
            fail(
                f"{name}'s packaged manifest requires {dependency_name} {dependency.get('req')!r}, "
                f"expected ^{version}"
            )
        if dependency.get("source") != "registry+https://github.com/rust-lang/crates.io-index":
            fail(
                f"{name}'s packaged manifest does not source {dependency_name} from crates.io"
            )


def package_archive(
    package: dict[str, object], version: str, commit: str, target_dir: Path
) -> dict[str, object]:
    name = str(package["name"])
    filename = f"{name}-{version}.crate"
    archive = target_dir / "package" / filename
    if not archive.is_file():
        fail(f"cargo package did not create {archive}")
    root = f"{name}-{version}"
    with tarfile.open(archive, "r:gz") as bundle:
        names = set(bundle.getnames())
        vcs_name = f"{root}/.cargo_vcs_info.json"
        manifest_name = f"{root}/Cargo.toml"
        lock_name = f"{root}/Cargo.lock"
        for required in (vcs_name, manifest_name, lock_name):
            if required not in names:
                fail(f"{filename} does not contain {required}")
        vcs_file = bundle.extractfile(vcs_name)
        manifest_file = bundle.extractfile(manifest_name)
        if vcs_file is None or manifest_file is None:
            fail(f"failed to read release metadata from {filename}")
        vcs = json.load(vcs_file)
        vcs_sha = vcs.get("git", {}).get("sha1")
        if vcs_sha != commit:
            fail(f"{filename} records VCS SHA {vcs_sha!r}, expected {commit}")
        manifest_bytes = manifest_file.read()

        readme_sha = None
        readme = package.get("readme")
        if readme:
            manifest_dir = Path(str(package["manifest_path"])).parent
            readme_path = Path(str(readme))
            if not readme_path.is_absolute():
                readme_path = manifest_dir / readme_path
            relative = readme_path.relative_to(manifest_dir)
            member_name = f"{root}/{relative.as_posix()}"
            if member_name not in names:
                fail(f"{filename} does not contain declared README {relative}")
            readme_file = bundle.extractfile(member_name)
            if readme_file is None:
                fail(f"failed to read {member_name} from {filename}")
            packaged_readme = readme_file.read()
            source_readme = readme_path.read_bytes()
            if packaged_readme != source_readme:
                fail(f"{filename}'s README is not byte-identical to {readme_path}")
            readme_sha = sha256_bytes(packaged_readme)

        extracted_manifest = extract_package_for_manifest(bundle, target_dir, root)

    if extracted_manifest.read_bytes() != manifest_bytes:
        fail(f"{filename}'s extracted Cargo.toml differs from the archived manifest bytes")
    validate_packaged_manifest(
        read_packaged_manifest(extracted_manifest, target_dir), name, version
    )

    return {
        "name": name,
        "version": version,
        "archive": filename,
        "sha256": sha256_file(archive),
        "size": archive.stat().st_size,
        "vcs_sha": commit,
        "manifest_sha256": sha256_bytes(manifest_bytes),
        "readme_sha256": readme_sha,
    }


def create_package_manifest(
    version: str,
    commit: str,
    lock_sha256: str,
    target_dir: Path,
    *,
    expected: dict[str, object] | None = None,
) -> dict[str, object]:
    metadata = cargo_metadata(target_dir)
    by_name = verify_workspace(metadata, version)
    package_dir = target_dir / "package"
    package_dir.mkdir(parents=True, exist_ok=True)
    for name in PUBLISH_ORDER:
        candidate = package_dir / f"{name}-{version}.crate"
        if candidate.exists():
            candidate.unlink()
    command = cargo_args("package", "--locked", "--no-verify")
    for name in PUBLISH_ORDER:
        command.extend(("-p", name))
    lock_before = sha256_file(Path("Cargo.lock"))
    if lock_before != lock_sha256:
        fail(f"root lock checksum is {lock_before}, expected {lock_sha256}")
    run(command, env=cargo_env(target_dir))
    lock_after = sha256_file(Path("Cargo.lock"))
    if lock_after != lock_sha256:
        fail("cargo package changed the approved root Cargo.lock")
    manifest = {
        "schema": 1,
        "repository": REPOSITORY,
        "version": version,
        "commit": commit,
        "toolchain": TOOLCHAIN,
        "lock_sha256": lock_sha256,
        "packages": [
            package_archive(by_name[name], version, commit, target_dir)
            for name in PUBLISH_ORDER
        ],
    }
    if expected is not None and manifest != expected:
        fail("package archives are not byte-identical to the preflight package manifest")
    for package in manifest["packages"]:
        print(f"ok: {package['archive']} sha256={package['sha256']}")
    return manifest


def encode_evidence(data: bytes) -> str:
    return base64.b64encode(data).decode("ascii")


def decode_evidence(encoded: str, expected_sha256: str, label: str) -> bytes:
    if not SHA256_RE.fullmatch(expected_sha256):
        fail(f"{label} checksum is not a lowercase SHA-256")
    try:
        data = base64.b64decode(encoded, validate=True)
    except (ValueError, base64.binascii.Error) as error:
        raise ReleaseError(f"{label} is not valid base64") from error
    actual = sha256_bytes(data)
    if actual != expected_sha256:
        fail(f"{label} checksum is {actual}, expected {expected_sha256}")
    return data


def emit_github_outputs(values: dict[str, str]) -> None:
    output_path = os.environ.get("GITHUB_OUTPUT")
    if not output_path:
        fail("GITHUB_OUTPUT is unset")
    for key, value in values.items():
        if "\n" in key or "\n" in value:
            fail(f"GitHub output {key!r} must be a single line")
    utf16_size = sum(len(f"{key}={value}\n".encode("utf-16-le")) for key, value in values.items())
    if utf16_size > MAX_GITHUB_OUTPUT_UTF16_BYTES:
        fail(
            f"GitHub outputs require {utf16_size} UTF-16 bytes, exceeding "
            f"the {MAX_GITHUB_OUTPUT_UTF16_BYTES}-byte release ceiling"
        )
    with Path(output_path).open("a", encoding="utf-8") as handle:
        for key, value in values.items():
            handle.write(f"{key}={value}\n")
    print(f"ok: emitted {len(values)} outputs using {utf16_size} UTF-16 bytes")


def sparse_index_path(name: str) -> str:
    lowered = name.lower()
    length = len(lowered)
    if length == 1:
        return f"1/{lowered}"
    if length == 2:
        return f"2/{lowered}"
    if length == 3:
        return f"3/{lowered[0]}/{lowered}"
    return f"{lowered[:2]}/{lowered[2:4]}/{lowered}"


def parse_sparse_checksum(text: str, name: str, version: str) -> str | None:
    for raw_line in text.splitlines():
        if not raw_line.strip():
            continue
        try:
            entry = json.loads(raw_line)
        except json.JSONDecodeError as error:
            raise ReleaseError(f"invalid sparse-index JSON for {name}: {error}") from error
        if entry.get("vers") == version:
            if entry.get("name") != name:
                fail(f"sparse index entry names {entry.get('name')!r}, expected {name!r}")
            checksum = entry.get("cksum")
            if not isinstance(checksum, str) or not SHA256_RE.fullmatch(checksum):
                fail(f"sparse index has an invalid checksum for {name}@{version}")
            return checksum
    return None


def classify_version(
    name: str,
    version: str,
    record: dict[str, object],
    expected_checksum: str,
    commit: str,
    *,
    allow_bootstrap: bool,
) -> str:
    checksum = record.get("checksum")
    if checksum != expected_checksum:
        fail(
            f"{name}@{version} registry checksum is {checksum!r}, expected {expected_checksum}"
        )
    trustpub = record.get("trustpub_data")
    published_by = record.get("published_by")
    if published_by is None and isinstance(trustpub, dict):
        required = {
            "provider": "github",
            "repository": REPOSITORY,
            "sha": commit,
        }
        for key, value in required.items():
            if trustpub.get(key) != value:
                fail(
                    f"{name}@{version} trusted-publishing {key} is "
                    f"{trustpub.get(key)!r}, expected {value!r}"
                )
        run_id = trustpub.get("run_id")
        if not isinstance(run_id, str) or not run_id.isdigit():
            fail(f"{name}@{version} has no valid trusted-publishing run_id")
        return "oidc"
    if allow_bootstrap and version == "0.0.1" and name in BOOTSTRAP_0_0_1:
        login = published_by.get("login") if isinstance(published_by, dict) else None
        if login == "al8n" and trustpub is None:
            return "bootstrap"
    fail(
        f"{name}@{version} has unacceptable provenance; only exact GitHub OIDC provenance is "
        "accepted, except the five al8n-published 0.0.1 bootstrap siblings"
    )


class RegistryClient:
    def __init__(self, http: HttpClient | None = None) -> None:
        self.http = http or HttpClient()

    def crate(self, name: str) -> dict[str, object] | None:
        url = f"https://crates.io/api/v1/crates/{urllib.parse.quote(name)}"
        try:
            payload = self.http.request_json("GET", url)
        except HttpStatusError as error:
            if error.status == 404:
                return None
            raise
        if not isinstance(payload, dict) or not isinstance(payload.get("crate"), dict):
            fail(f"crates.io returned an invalid crate response for {name}")
        return payload["crate"]

    def version(self, name: str, version: str) -> dict[str, object] | None:
        url = (
            "https://crates.io/api/v1/crates/"
            f"{urllib.parse.quote(name)}/{urllib.parse.quote(version)}"
        )
        try:
            payload = self.http.request_json("GET", url)
        except HttpStatusError as error:
            if error.status == 404:
                return None
            raise
        if not isinstance(payload, dict) or not isinstance(payload.get("version"), dict):
            fail(f"crates.io returned an invalid version response for {name}@{version}")
        return payload["version"]

    def sparse_checksum(self, name: str, version: str) -> str | None:
        cache_key = urllib.parse.urlencode({"release-check": version})
        url = f"https://index.crates.io/{sparse_index_path(name)}?{cache_key}"
        try:
            data = self.http.request_bytes("GET", url, accept="text/plain")
        except HttpStatusError as error:
            if error.status == 404:
                return None
            raise
        return parse_sparse_checksum(data.decode(), name, version)

    def archive(self, name: str, version: str) -> bytes | None:
        filename = f"{name}-{version}.crate"
        url = (
            f"https://static.crates.io/crates/{urllib.parse.quote(name)}/"
            f"{urllib.parse.quote(filename)}"
        )
        try:
            return self.http.request_bytes("GET", url, accept="application/octet-stream")
        except HttpStatusError as error:
            if error.status == 404:
                return None
            raise


def verify_registry_once(
    registry: RegistryClient,
    package: dict[str, object],
    commit: str,
    *,
    allow_bootstrap: bool,
) -> dict[str, object] | None:
    name = str(package["name"])
    version = str(package["version"])
    expected = str(package["sha256"])
    record = registry.version(name, version)
    if record is None:
        return None
    provenance = classify_version(
        name,
        version,
        record,
        expected,
        commit,
        allow_bootstrap=allow_bootstrap,
    )
    index_checksum = registry.sparse_checksum(name, version)
    if index_checksum is None:
        return None
    if index_checksum != expected:
        fail(
            f"{name}@{version} sparse-index checksum is {index_checksum}, expected {expected}"
        )
    archive = registry.archive(name, version)
    if archive is None:
        return None
    download_checksum = sha256_bytes(archive)
    if download_checksum != expected:
        fail(
            f"{name}@{version} downloaded archive checksum is {download_checksum}, expected {expected}"
        )
    published_by = record.get("published_by")
    return {
        **package,
        "provenance": provenance,
        "api_checksum": record.get("checksum"),
        "index_checksum": index_checksum,
        "download_checksum": download_checksum,
        "published_by": (
            published_by.get("login") if isinstance(published_by, dict) else None
        ),
        "trustpub_data": record.get("trustpub_data"),
    }


def poll_registry(
    registry: RegistryClient,
    package: dict[str, object],
    commit: str,
    *,
    allow_bootstrap: bool,
    timeout: float,
    sleep: Callable[[float], None] = time.sleep,
    monotonic: Callable[[], float] = time.monotonic,
) -> dict[str, object]:
    deadline = monotonic() + timeout
    delay = 1.0
    last_error = "version record is absent"
    while True:
        try:
            evidence = verify_registry_once(
                registry, package, commit, allow_bootstrap=allow_bootstrap
            )
            if evidence is not None:
                return evidence
            last_error = "API, sparse index, or archive has not propagated"
        except HttpStatusError as error:
            if error.status < 500 and error.status != 429:
                raise
            last_error = str(error)
        except urllib.error.URLError as error:
            last_error = str(error)
        now = monotonic()
        if now >= deadline:
            name = package["name"]
            version = package["version"]
            fail(f"timed out verifying {name}@{version}: {last_error}")
        sleep(min(delay, max(0.0, deadline - now)))
        delay = min(delay * 1.7, 15.0)


def package_by_name(manifest: dict[str, object]) -> dict[str, dict[str, object]]:
    packages = manifest.get("packages")
    if not isinstance(packages, list):
        fail("package manifest has no packages list")
    result = {}
    for package in packages:
        if not isinstance(package, dict) or not isinstance(package.get("name"), str):
            fail("package manifest contains an invalid package")
        result[package["name"]] = package
    if set(result) != set(PUBLISH_ORDER):
        fail(f"package manifest names are {sorted(result)}, expected {list(PUBLISH_ORDER)}")
    return result


def verify_manifest_header(
    manifest: dict[str, object], version: str, commit: str, lock_sha256: str
) -> None:
    expected = {
        "schema": 1,
        "repository": REPOSITORY,
        "version": version,
        "commit": commit,
        "toolchain": TOOLCHAIN,
        "lock_sha256": lock_sha256,
    }
    for key, value in expected.items():
        if manifest.get(key) != value:
            fail(f"package manifest {key} is {manifest.get(key)!r}, expected {value!r}")
    package_by_name(manifest)


def local_archive_path(target_dir: Path, package: dict[str, object]) -> Path:
    return target_dir / "package" / str(package["archive"])


def verify_local_archive(target_dir: Path, package: dict[str, object]) -> None:
    path = local_archive_path(target_dir, package)
    if not path.is_file():
        fail(f"local package archive is missing: {path}")
    checksum = sha256_file(path)
    if checksum != package.get("sha256"):
        fail(f"local archive {path.name} checksum is {checksum}, expected {package.get('sha256')}")
    if path.stat().st_size != package.get("size"):
        fail(f"local archive {path.name} size changed")


def require_trustpub_only(crate: dict[str, object], name: str) -> None:
    if crate.get("trustpub_only") is not True:
        fail(f"{name} does not enforce Trusted-Publishing-only mode")


def decide_registry_action(
    name: str,
    version: str,
    krate: dict[str, object] | None,
    record: dict[str, object] | None,
    checksum: str,
    commit: str,
) -> str:
    if krate is None:
        fail(
            f"{name} does not exist on crates.io; trusted publishing cannot create crate "
            "names, so the manual bootstrap is required"
        )
    require_trustpub_only(krate, name)
    if record is None:
        return "publish"
    classify_version(
        name,
        version,
        record,
        checksum,
        commit,
        allow_bootstrap=True,
    )
    return "skip"


def authoritative_publish_result(
    name: str,
    returncode: int,
    evidence: dict[str, object] | None,
    verification_error: ReleaseError | None,
) -> dict[str, object]:
    if evidence is not None:
        return evidence
    if verification_error is None:
        fail(f"{name}: publication produced neither evidence nor an error")
    if returncode != 0:
        raise ReleaseError(
            f"cargo publish for {name} exited {returncode}, and the registry did not prove "
            f"an exact OIDC upload: {verification_error}"
        ) from verification_error
    raise verification_error


def publish_packages(
    manifest: dict[str, object],
    target_dir: Path,
    commit: str,
    poll_timeout: float,
) -> dict[str, object]:
    if not os.environ.get("CARGO_REGISTRY_TOKEN"):
        fail("CARGO_REGISTRY_TOKEN is unset; the protected OIDC exchange did not run")
    registry = RegistryClient()
    packages = package_by_name(manifest)
    evidence = []
    for name in PUBLISH_ORDER:
        package = packages[name]
        verify_local_archive(target_dir, package)
        krate = registry.crate(name)
        existing = registry.version(name, str(package["version"]))
        action = decide_registry_action(
            name,
            str(package["version"]),
            krate,
            existing,
            str(package["sha256"]),
            commit,
        )
        if action == "skip":
            print(f"ok: {name}@{package['version']} already exists exactly; skipping upload")
            item = poll_registry(
                registry,
                package,
                commit,
                allow_bootstrap=True,
                timeout=poll_timeout,
            )
            evidence.append(item)
            continue

        result = run(
            cargo_args(
                "publish",
                "--locked",
                "--registry",
                "crates-io",
                "-p",
                name,
            ),
            check=False,
            env=cargo_env(target_dir, allow_token=True),
        )
        item = None
        verification_error = None
        try:
            item = poll_registry(
                registry,
                package,
                commit,
                allow_bootstrap=False,
                timeout=poll_timeout,
            )
        except ReleaseError as error:
            verification_error = error
        item = authoritative_publish_result(
            name, result.returncode, item, verification_error
        )
        if result.returncode != 0:
            print(
                f"warning: cargo publish for {name} exited {result.returncode}, but the registry "
                "authoritatively contains the exact OIDC archive",
                file=sys.stderr,
            )
        verify_local_archive(target_dir, package)
        evidence.append(item)
        print(f"ok: {name}@{package['version']} is visible in API, index, and archive storage")

    return {
        "schema": 1,
        "repository": REPOSITORY,
        "version": manifest["version"],
        "commit": commit,
        "toolchain": TOOLCHAIN,
        "lock_sha256": manifest["lock_sha256"],
        "packages": evidence,
    }


def verify_all_registry(
    manifest: dict[str, object], commit: str, poll_timeout: float
) -> dict[str, object]:
    registry = RegistryClient()
    packages = package_by_name(manifest)
    evidence = []
    for name in PUBLISH_ORDER:
        krate = registry.crate(name)
        if krate is None:
            fail(f"{name} disappeared from crates.io")
        require_trustpub_only(krate, name)
        evidence.append(
            poll_registry(
                registry,
                packages[name],
                commit,
                allow_bootstrap=True,
                timeout=poll_timeout,
            )
        )
    return {
        "schema": 1,
        "repository": REPOSITORY,
        "version": manifest["version"],
        "commit": commit,
        "toolchain": TOOLCHAIN,
        "lock_sha256": manifest["lock_sha256"],
        "packages": evidence,
    }


class GitHubReleaseClient:
    def __init__(self, token: str) -> None:
        self.http = HttpClient(token)

    def get_or_none(self, path: str) -> object | None:
        try:
            return self.http.request_json("GET", github_api_url(path))
        except HttpStatusError as error:
            if error.status == 404:
                return None
            raise

    def paginated_list(self, path: str) -> list[object]:
        items: list[object] = []
        for page in range(1, 101):
            separator = "&" if "?" in path else "?"
            payload = self.http.request_json(
                "GET",
                github_api_url(f"{path}{separator}per_page=100&page={page}"),
            )
            if not isinstance(payload, list):
                fail(f"GitHub returned a non-list response for {path}")
            items.extend(payload)
            if len(payload) < 100:
                return items
        fail(f"GitHub pagination for {path} exceeded 10,000 records")

    def find_release(self, tag: str) -> dict[str, object] | None:
        return select_release_by_tag(
            self.paginated_list(f"repos/{REPOSITORY}/releases"), tag
        )

    def ensure_annotated_tag(self, tag: str, commit: str, message: str) -> None:
        encoded_tag = urllib.parse.quote(tag, safe="")
        ref = self.get_or_none(f"repos/{REPOSITORY}/git/ref/tags/{encoded_tag}")
        if ref is None:
            tag_object = self.http.request_json(
                "POST",
                github_api_url(f"repos/{REPOSITORY}/git/tags"),
                value={"tag": tag, "message": message, "object": commit, "type": "commit"},
            )
            if not isinstance(tag_object, dict) or not isinstance(tag_object.get("sha"), str):
                fail("GitHub did not return an annotated tag object SHA")
            try:
                self.http.request_json(
                    "POST",
                    github_api_url(f"repos/{REPOSITORY}/git/refs"),
                    value={"ref": f"refs/tags/{tag}", "sha": tag_object["sha"]},
                )
            except HttpStatusError as error:
                if error.status != 422:
                    raise
            ref = self.get_or_none(f"repos/{REPOSITORY}/git/ref/tags/{encoded_tag}")
        self.verify_annotated_tag(ref, tag, commit, message)

    def verify_annotated_tag(
        self, ref: object, tag: str, commit: str, message: str
    ) -> None:
        if not isinstance(ref, dict) or not isinstance(ref.get("object"), dict):
            fail(f"GitHub returned an invalid ref for {tag}")
        ref_object = ref["object"]
        if ref_object.get("type") != "tag":
            fail(f"{tag} exists but is not an annotated tag; it will not be moved or replaced")
        tag_sha = ref_object.get("sha")
        tag_object = self.http.request_json(
            "GET", github_api_url(f"repos/{REPOSITORY}/git/tags/{tag_sha}")
        )
        if not isinstance(tag_object, dict) or not isinstance(tag_object.get("object"), dict):
            fail(f"GitHub returned an invalid annotated tag object for {tag}")
        if tag_object.get("tag") != tag:
            fail(f"annotated tag object names {tag_object.get('tag')!r}, expected {tag!r}")
        if tag_object.get("message") != message:
            fail(f"{tag} has a different annotation and will not be moved or replaced")
        target = tag_object["object"]
        if target.get("type") != "commit" or target.get("sha") != commit:
            fail(
                f"{tag} points to {target.get('type')} {target.get('sha')}, expected commit {commit}"
            )

    def ensure_release(self, tag: str, commit: str, name: str, body: str) -> dict[str, object]:
        # GET /releases/tags/{tag} is a published-release endpoint and does not
        # provide reliable draft recovery. The authenticated list includes drafts.
        release = self.find_release(tag)
        if release is None:
            try:
                release = self.http.request_json(
                    "POST",
                    github_api_url(f"repos/{REPOSITORY}/releases"),
                    value={
                        "tag_name": tag,
                        "target_commitish": commit,
                        "name": name,
                        "body": body,
                        # Keep an interrupted upload invisible. Assets are made exact before the
                        # controller changes this draft into the public release.
                        "draft": True,
                        "prerelease": False,
                    },
                )
            except HttpStatusError as error:
                if error.status != 422:
                    raise
                release = self.find_release(tag)
                if release is None:
                    raise
        if not isinstance(release, dict):
            fail("GitHub returned an invalid release")
        required = {
            "tag_name": tag,
            "name": name,
            "body": body,
            "prerelease": False,
        }
        validate_release_fields(release, required)
        if release.get("draft") not in (True, False):
            fail("existing GitHub Release has no valid draft state")
        return release

    def publish_release(self, release: dict[str, object], required: dict[str, object]) -> dict[str, object]:
        if release.get("draft") is True:
            release_id = release.get("id")
            if not isinstance(release_id, int):
                fail("draft GitHub Release has no numeric id")
            release = self.http.request_json(
                "PATCH",
                github_api_url(f"repos/{REPOSITORY}/releases/{release_id}"),
                value={"draft": False},
            )
            if not isinstance(release, dict):
                fail("GitHub returned an invalid published release")
        validate_release_fields(release, {**required, "draft": False})
        return release

    def ensure_assets(self, release: dict[str, object], assets: list[Path]) -> None:
        release_id = release.get("id")
        if not isinstance(release_id, int):
            fail("GitHub Release has no numeric id")
        expected = {path.name: path for path in assets}
        if len(expected) != len(assets):
            fail("release asset inputs contain duplicate names")
        existing = index_release_assets(
            self.paginated_list(f"repos/{REPOSITORY}/releases/{release_id}/assets"),
            set(expected),
        )
        for path in assets:
            data = path.read_bytes()
            asset = existing.get(path.name)
            if asset is None:
                query = urllib.parse.urlencode({"name": path.name})
                url = (
                    f"https://uploads.github.com/repos/{REPOSITORY}/releases/"
                    f"{release_id}/assets?{query}"
                )
                self.http.request_json_bytes(url, data)

        # Asset rows and digests can lag the successful upload response. Wait
        # briefly, but never delete, replace, or silently retain an extra asset.
        deadline = time.monotonic() + 60
        while True:
            existing = index_release_assets(
                self.paginated_list(f"repos/{REPOSITORY}/releases/{release_id}/assets"),
                set(expected),
            )
            missing = set(expected) - set(existing)
            pending = [name for name, asset in existing.items() if not asset.get("digest")]
            if not missing and not pending:
                break
            if time.monotonic() >= deadline:
                fail(
                    "GitHub Release assets did not become complete with server digests: "
                    f"missing={sorted(missing)}, pending={sorted(pending)}"
                )
            time.sleep(2)

        if set(existing) != set(expected):
            fail("GitHub Release asset set changed while it was being verified")
        for name, path in expected.items():
            data = path.read_bytes()
            digest = sha256_bytes(data)
            self.verify_asset(existing[name], digest, len(data), name)
            print(f"ok: GitHub Release asset {name} sha256={digest}")

    def verify_asset(
        self, asset: object, expected_sha256: str, expected_size: int, name: str
    ) -> None:
        if not isinstance(asset, dict):
            fail(f"GitHub returned invalid metadata for release asset {name}")
        if asset.get("name") != name or asset.get("size") != expected_size:
            fail(f"existing release asset {name} has different metadata and will not be replaced")
        digest = asset.get("digest")
        if not isinstance(digest, str):
            fail(
                f"GitHub has not exposed a server digest for release asset {name}; rerun after "
                "asset processing completes"
            )
        if digest != f"sha256:{expected_sha256}":
            fail(
                f"existing release asset {name} has digest {digest}, "
                f"expected sha256:{expected_sha256}"
            )

def validate_release_fields(
    release: dict[str, object], required: dict[str, object]
) -> None:
    for key, value in required.items():
        if release.get(key) != value:
            fail(
                f"existing GitHub Release {key} is {release.get(key)!r}, expected {value!r}; "
                "it will not be overwritten"
            )


def select_release_by_tag(
    releases: list[object], tag: str
) -> dict[str, object] | None:
    matches = [
        release
        for release in releases
        if isinstance(release, dict) and release.get("tag_name") == tag
    ]
    if len(matches) > 1:
        fail(f"GitHub returned multiple releases, including drafts, for tag {tag}")
    if not matches:
        return None
    return matches[0]


def index_release_assets(
    assets: list[object], expected_names: set[str]
) -> dict[str, dict[str, object]]:
    indexed: dict[str, dict[str, object]] = {}
    for asset in assets:
        if not isinstance(asset, dict) or not isinstance(asset.get("name"), str):
            fail("GitHub returned invalid release asset metadata")
        name = asset["name"]
        if name not in expected_names:
            fail(f"GitHub Release contains unexpected asset {name!r}; it will not be deleted")
        if name in indexed:
            fail(f"GitHub Release contains duplicate asset name {name!r}")
        indexed[name] = asset
    return indexed


def changelog_section(version: str) -> str:
    lines = Path("CHANGELOG.md").read_text(encoding="utf-8").splitlines()
    marker = f"## [{version}]"
    start = next((index for index, line in enumerate(lines) if line.startswith(marker)), None)
    if start is None:
        fail(f"CHANGELOG.md has no {marker} section")
    end = next(
        (index for index in range(start + 1, len(lines)) if lines[index].startswith("## [")),
        len(lines),
    )
    section = "\n".join(lines[start:end]).strip() + "\n"
    if len(section.splitlines()) < 2:
        fail(f"CHANGELOG.md section for {version} is empty")
    return section


def build_release_files(
    package_manifest: dict[str, object],
    registry_manifest: dict[str, object],
    target_dir: Path,
    evidence_dir: Path,
) -> tuple[dict[str, object], list[Path]]:
    # The registry manifest extends, and may not alter, each local package row.
    local = package_by_name(package_manifest)
    registry_rows = package_by_name(registry_manifest)
    for name in PUBLISH_ORDER:
        for key, value in local[name].items():
            if registry_rows[name].get(key) != value:
                fail(f"registry evidence changed local package field {name}.{key}")
    evidence_dir.mkdir(parents=True, exist_ok=True)
    release_manifest = {
        "schema": 1,
        "repository": REPOSITORY,
        "version": package_manifest["version"],
        "commit": package_manifest["commit"],
        "tag": f"v{package_manifest['version']}",
        "toolchain": TOOLCHAIN,
        "lock_sha256": package_manifest["lock_sha256"],
        "preflight_manifest_sha256": sha256_bytes(canonical_json_bytes(package_manifest)),
        "trusted_publisher": {
            "repository": REPOSITORY,
            "workflow": WORKFLOW_FILENAME,
            "environment": TRUSTED_ENVIRONMENT,
        },
        "packages": registry_manifest["packages"],
    }
    manifest_path = evidence_dir / "release-manifest.json"
    write_json(manifest_path, release_manifest)
    sum_lines = []
    archive_paths = []
    for package in package_manifest["packages"]:
        archive = local_archive_path(target_dir, package)
        verify_local_archive(target_dir, package)
        archive_paths.append(archive)
        sum_lines.append(f"{package['sha256']}  {archive.name}")
    sums_path = evidence_dir / "SHA256SUMS"
    sums_path.write_text("\n".join(sum_lines) + "\n", encoding="utf-8")
    return release_manifest, [*archive_paths, sums_path, manifest_path]


def cmd_verify(args: argparse.Namespace) -> None:
    verify_event_context(args.version, args.commit)
    verify_git_commit(args.commit)
    token = os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN")
    if args.github_gates:
        if not token:
            fail("GH_TOKEN/GITHUB_TOKEN is required to verify workflow gates")
        verify_github_gates(HttpClient(token), args.commit)
    if not args.no_workspace:
        metadata = cargo_metadata(Path(args.target_dir))
        verify_workspace(metadata, args.version)


def cmd_prepare(args: argparse.Namespace) -> None:
    verify_event_context(args.version, args.commit)
    verify_git_commit(args.commit)
    if Path("Cargo.lock").exists():
        fail("root Cargo.lock already exists; preflight must generate the ignored lock itself")
    target_dir = Path(args.target_dir).resolve()
    run(cargo_args("generate-lockfile"), env=cargo_env(target_dir))
    verify_workspace(cargo_metadata(target_dir), args.version)
    # Cargo 1.99.1 cannot dry-run this unpublished DAG: it creates all six archives, then its
    # temporary registry reports "no hash listed" when smear-parser tries to resolve the local
    # smear-lexer archive. Exact-SHA CI supplies the build/test verdict. This step records the
    # bytes with locked/no-verify packaging; real `cargo publish --locked` performs Cargo's
    # verification later, after each dependency is publicly indexed.
    lock_bytes = Path("Cargo.lock").read_bytes()
    lock_sha = sha256_bytes(lock_bytes)
    package_manifest = create_package_manifest(
        args.version, args.commit, lock_sha, target_dir
    )
    manifest_bytes = canonical_json_bytes(package_manifest)
    emit_github_outputs(
        {
            "lock_b64": encode_evidence(lock_bytes),
            "lock_sha256": lock_sha,
            "preflight_manifest_b64": encode_evidence(manifest_bytes),
            "preflight_manifest_sha256": sha256_bytes(manifest_bytes),
        }
    )


def cmd_restore(args: argparse.Namespace) -> None:
    if Path("Cargo.lock").exists():
        fail("refusing to replace an existing root Cargo.lock")
    lock_encoded = os.environ.get(args.lock_env)
    manifest_encoded = os.environ.get(args.manifest_env)
    if lock_encoded is None or manifest_encoded is None:
        fail("release evidence environment variables are missing")
    lock = decode_evidence(lock_encoded, args.lock_sha256, "root Cargo.lock")
    manifest = decode_evidence(
        manifest_encoded, args.manifest_sha256, "preflight package manifest"
    )
    Path("Cargo.lock").write_bytes(lock)
    output = Path(args.manifest_output)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_bytes(manifest)
    print(f"ok: restored root Cargo.lock sha256={args.lock_sha256}")


def cmd_restore_json(args: argparse.Namespace) -> None:
    encoded = os.environ.get(args.encoded_env)
    if encoded is None:
        fail(f"{args.encoded_env} is unset")
    data = decode_evidence(encoded, args.sha256, args.label)
    output = Path(args.output)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_bytes(data)


def cmd_package(args: argparse.Namespace) -> None:
    expected_value = load_json(Path(args.expected_manifest))
    if not isinstance(expected_value, dict):
        fail("preflight package manifest is not an object")
    verify_manifest_header(expected_value, args.version, args.commit, args.lock_sha256)
    manifest = create_package_manifest(
        args.version,
        args.commit,
        args.lock_sha256,
        Path(args.target_dir).resolve(),
        expected=expected_value,
    )
    if args.output:
        write_json(Path(args.output), manifest)


def cmd_publish(args: argparse.Namespace) -> None:
    value = load_json(Path(args.package_manifest))
    if not isinstance(value, dict):
        fail("package manifest is not an object")
    verify_manifest_header(value, args.version, args.commit, args.lock_sha256)
    registry_manifest = publish_packages(
        value, Path(args.target_dir).resolve(), args.commit, args.poll_timeout
    )
    output = Path(args.output)
    write_json(output, registry_manifest)
    data = canonical_json_bytes(registry_manifest)
    emit_github_outputs(
        {
            "registry_manifest_b64": encode_evidence(data),
            "registry_manifest_sha256": sha256_bytes(data),
        }
    )


def cmd_release(args: argparse.Namespace) -> None:
    package_value = load_json(Path(args.package_manifest))
    registry_value = load_json(Path(args.registry_manifest))
    if not isinstance(package_value, dict) or not isinstance(registry_value, dict):
        fail("release manifests must be objects")
    verify_manifest_header(package_value, args.version, args.commit, args.lock_sha256)
    live_registry = verify_all_registry(package_value, args.commit, args.poll_timeout)
    if live_registry != registry_value:
        fail("public registry evidence changed between publication and GitHub Release")
    release_manifest, assets = build_release_files(
        package_value,
        registry_value,
        Path(args.target_dir).resolve(),
        Path(args.evidence_dir).resolve(),
    )
    token = os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN")
    if not token:
        fail("GH_TOKEN/GITHUB_TOKEN is required to create the tag and GitHub Release")
    tag = str(release_manifest["tag"])
    section = changelog_section(args.version)
    message = f"Smear {args.version}\n\n{section}"
    github = GitHubReleaseClient(token)
    github.ensure_annotated_tag(tag, args.commit, message)
    release = github.ensure_release(tag, args.commit, f"Smear {args.version}", section)
    github.ensure_assets(release, assets)
    release = github.publish_release(
        release,
        {
            "tag_name": tag,
            "name": f"Smear {args.version}",
            "body": section,
            "prerelease": False,
        },
    )
    print(f"ok: {tag} and its GitHub Release are exact and complete")


class _FakeClock:
    def __init__(self) -> None:
        self.now = 0.0

    def monotonic(self) -> float:
        return self.now

    def sleep(self, seconds: float) -> None:
        self.now += seconds


class _FakeRegistry:
    def __init__(
        self,
        *,
        record: dict[str, object] | None,
        index: str | None,
        archive: bytes | None,
    ) -> None:
        self.record = record
        self.index = index
        self.archive_bytes = archive

    def version(self, _name: str, _version: str) -> dict[str, object] | None:
        return self.record

    def sparse_checksum(self, _name: str, _version: str) -> str | None:
        return self.index

    def archive(self, _name: str, _version: str) -> bytes | None:
        return self.archive_bytes


class ReleaseSelfTests(unittest.TestCase):
    commit = "a" * 40
    checksum = sha256_bytes(b"crate")

    def oidc_record(self, **changes: object) -> dict[str, object]:
        value: dict[str, object] = {
            "checksum": self.checksum,
            "published_by": None,
            "trustpub_data": {
                "provider": "github",
                "repository": REPOSITORY,
                "run_id": "12345",
                "sha": self.commit,
            },
        }
        value.update(changes)
        return value

    def package(self) -> dict[str, object]:
        return {
            "name": "smear",
            "version": "0.0.1",
            "sha256": self.checksum,
        }

    def cargo_manifest(self, name: str) -> dict[str, object]:
        dependencies = [
            {
                "name": dependency,
                "source": "registry+https://github.com/rust-lang/crates.io-index",
                "req": "^0.0.1",
                "kind": None,
            }
            for dependency in sorted(INTERNAL_EDGES[name])
        ]
        return {"name": name, "version": "0.0.1", "dependencies": dependencies}

    def test_oidc_exact_is_accepted(self) -> None:
        self.assertEqual(
            classify_version(
                "smear", "0.0.1", self.oidc_record(), self.checksum, self.commit,
                allow_bootstrap=True,
            ),
            "oidc",
        )

    def test_oidc_wrong_checksum_repository_sha_or_run_is_rejected(self) -> None:
        cases = [
            self.oidc_record(checksum="0" * 64),
            self.oidc_record(
                trustpub_data={
                    "provider": "github", "repository": "other/repo",
                    "run_id": "1", "sha": self.commit,
                }
            ),
            self.oidc_record(
                trustpub_data={
                    "provider": "github", "repository": REPOSITORY,
                    "run_id": "1", "sha": "b" * 40,
                }
            ),
            self.oidc_record(
                trustpub_data={
                    "provider": "github", "repository": REPOSITORY,
                    "run_id": "", "sha": self.commit,
                }
            ),
        ]
        for record in cases:
            with self.subTest(record=record), self.assertRaises(ReleaseError):
                classify_version(
                    "smear", "0.0.1", record, self.checksum, self.commit,
                    allow_bootstrap=True,
                )

    def test_bootstrap_exception_is_exact_and_never_facade(self) -> None:
        record = {
            "checksum": self.checksum,
            "published_by": {"login": "al8n"},
            "trustpub_data": None,
        }
        self.assertEqual(
            classify_version(
                "smear-lexer", "0.0.1", record, self.checksum, self.commit,
                allow_bootstrap=True,
            ),
            "bootstrap",
        )
        rejected = [
            ("smear", "0.0.1", record, True),
            ("smear-lexer", "0.0.2", record, True),
            ("smear-lexer", "0.0.1", record, False),
            (
                "smear-lexer", "0.0.1",
                {**record, "published_by": {"login": "someone-else"}}, True,
            ),
            (
                "smear-lexer", "0.0.1",
                {**record, "trustpub_data": {"provider": "github"}}, True,
            ),
        ]
        for name, version, candidate, allow in rejected:
            with self.subTest(name=name, version=version, allow=allow), self.assertRaises(ReleaseError):
                classify_version(
                    name, version, candidate, self.checksum, self.commit,
                    allow_bootstrap=allow,
                )

    def test_registry_state_requires_existing_locked_name(self) -> None:
        with self.assertRaisesRegex(ReleaseError, "manual bootstrap"):
            decide_registry_action(
                "smear-lexer", "0.0.1", None, None, self.checksum, self.commit
            )
        with self.assertRaisesRegex(ReleaseError, "Trusted-Publishing-only"):
            decide_registry_action(
                "smear-lexer",
                "0.0.1",
                {"trustpub_only": False},
                None,
                self.checksum,
                self.commit,
            )
        self.assertEqual(
            decide_registry_action(
                "smear-lexer",
                "0.0.1",
                {"trustpub_only": True},
                None,
                self.checksum,
                self.commit,
            ),
            "publish",
        )
        self.assertEqual(
            decide_registry_action(
                "smear",
                "0.0.1",
                {"trustpub_only": True},
                self.oidc_record(),
                self.checksum,
                self.commit,
            ),
            "skip",
        )

    def test_cargo_manifest_reader_contract_is_exact(self) -> None:
        validate_packaged_manifest(self.cargo_manifest("smear-parser"), "smear-parser", "0.0.1")
        validate_packaged_manifest(self.cargo_manifest("smear"), "smear", "0.0.1")

        wrong_name = self.cargo_manifest("smear-parser")
        wrong_name["name"] = "other"
        with self.assertRaisesRegex(ReleaseError, "names"):
            validate_packaged_manifest(wrong_name, "smear-parser", "0.0.1")

        wrong_version = self.cargo_manifest("smear-parser")
        wrong_version["version"] = "0.0.2"
        with self.assertRaisesRegex(ReleaseError, "version"):
            validate_packaged_manifest(wrong_version, "smear-parser", "0.0.1")

        wrong_requirement = self.cargo_manifest("smear-parser")
        wrong_requirement["dependencies"][0]["req"] = "^0.0.2"
        with self.assertRaisesRegex(ReleaseError, r"expected \^0\.0\.1"):
            validate_packaged_manifest(wrong_requirement, "smear-parser", "0.0.1")

        wrong_source = self.cargo_manifest("smear-parser")
        wrong_source["dependencies"][0]["source"] = None
        with self.assertRaisesRegex(ReleaseError, "crates.io"):
            validate_packaged_manifest(wrong_source, "smear-parser", "0.0.1")

        missing_edge = self.cargo_manifest("smear-parser")
        missing_edge["dependencies"] = []
        with self.assertRaisesRegex(ReleaseError, "internal edges"):
            validate_packaged_manifest(missing_edge, "smear-parser", "0.0.1")

        self_dependency = self.cargo_manifest("smear")
        self_dependency["dependencies"].append(
            {"name": "smear", "source": None, "req": "*", "kind": "dev"}
        )
        with self.assertRaisesRegex(ReleaseError, "self dev-dependency"):
            validate_packaged_manifest(self_dependency, "smear", "0.0.1")

    def test_cargo_failure_is_overruled_only_by_exact_registry_evidence(self) -> None:
        evidence = {"name": "smear"}
        error = ReleaseError("registry absent")
        self.assertIs(
            authoritative_publish_result("smear", 101, evidence, None), evidence
        )
        with self.assertRaisesRegex(ReleaseError, "exited 101"):
            authoritative_publish_result("smear", 101, None, error)
        with self.assertRaisesRegex(ReleaseError, "registry absent"):
            authoritative_publish_result("smear", 0, None, error)

    def test_sparse_paths_and_parser(self) -> None:
        self.assertEqual(sparse_index_path("a"), "1/a")
        self.assertEqual(sparse_index_path("ab"), "2/ab")
        self.assertEqual(sparse_index_path("abc"), "3/a/abc")
        self.assertEqual(sparse_index_path("smear"), "sm/ea/smear")
        line = json.dumps({"name": "smear", "vers": "0.0.1", "cksum": self.checksum})
        self.assertEqual(parse_sparse_checksum(line, "smear", "0.0.1"), self.checksum)
        self.assertIsNone(parse_sparse_checksum(line, "smear", "0.0.2"))

    def test_evidence_tampering_is_rejected(self) -> None:
        encoded = encode_evidence(b"evidence")
        digest = sha256_bytes(b"evidence")
        self.assertEqual(decode_evidence(encoded, digest, "test"), b"evidence")
        with self.assertRaises(ReleaseError):
            decode_evidence(encoded, "0" * 64, "test")

    def test_output_limit_is_bounded(self) -> None:
        value = "x" * (MAX_GITHUB_OUTPUT_UTF16_BYTES // 2)
        size = len(f"x={value}\n".encode("utf-16-le"))
        self.assertGreater(size, MAX_GITHUB_OUTPUT_UTF16_BYTES)

    def test_four_way_registry_verification(self) -> None:
        registry = _FakeRegistry(
            record=self.oidc_record(), index=self.checksum, archive=b"crate"
        )
        evidence = verify_registry_once(
            registry, self.package(), self.commit, allow_bootstrap=False
        )
        self.assertIsNotNone(evidence)
        self.assertEqual(evidence["download_checksum"], self.checksum)

    def test_registry_mismatch_is_rejected(self) -> None:
        registry = _FakeRegistry(
            record=self.oidc_record(), index="0" * 64, archive=b"crate"
        )
        with self.assertRaises(ReleaseError):
            verify_registry_once(registry, self.package(), self.commit, allow_bootstrap=False)
        registry = _FakeRegistry(
            record=self.oidc_record(), index=self.checksum, archive=b"different"
        )
        with self.assertRaises(ReleaseError):
            verify_registry_once(registry, self.package(), self.commit, allow_bootstrap=False)

    def test_registry_propagation_timeout_is_loud(self) -> None:
        clock = _FakeClock()
        registry = _FakeRegistry(record=None, index=None, archive=None)
        with self.assertRaisesRegex(ReleaseError, "timed out"):
            poll_registry(
                registry,
                self.package(),
                self.commit,
                allow_bootstrap=False,
                timeout=2,
                sleep=clock.sleep,
                monotonic=clock.monotonic,
            )

    def test_github_gates_reject_missing_queued_and_failed(self) -> None:
        class FakeHttp:
            def __init__(self, runs: list[dict[str, object]]) -> None:
                self.runs = runs

            def request_json(self, _method: str, _url: str) -> object:
                return {"workflow_runs": self.runs}

        good = [
            {
                "id": index + 1,
                "path": path,
                "head_sha": self.commit,
                "event": "push",
                "status": "completed",
                "conclusion": "success",
            }
            for index, path in enumerate(REQUIRED_WORKFLOWS.values())
        ]
        verify_github_gates(FakeHttp(good), self.commit)  # type: ignore[arg-type]
        for runs in (
            good[:-1],
            [{**item, "status": "queued", "conclusion": None} for item in good],
            [{**item, "conclusion": "failure"} for item in good],
        ):
            with self.assertRaises(ReleaseError):
                verify_github_gates(FakeHttp(runs), self.commit)  # type: ignore[arg-type]

    def test_tag_verification_rejects_lightweight_and_wrong_commit(self) -> None:
        client = GitHubReleaseClient.__new__(GitHubReleaseClient)

        class FakeHttp:
            def __init__(self, target: str, message: str = "release") -> None:
                self.target = target
                self.message = message

            def request_json(self, _method: str, _url: str) -> object:
                return {
                    "tag": "v0.0.1",
                    "message": self.message,
                    "object": {"type": "commit", "sha": self.target},
                }

        client.http = FakeHttp(self.commit)  # type: ignore[assignment]
        client.verify_annotated_tag(
            {"object": {"type": "tag", "sha": "c" * 40}},
            "v0.0.1",
            self.commit,
            "release",
        )
        with self.assertRaises(ReleaseError):
            client.verify_annotated_tag(
                {"object": {"type": "commit", "sha": self.commit}},
                "v0.0.1",
                self.commit,
                "release",
            )
        client.http = FakeHttp("b" * 40)  # type: ignore[assignment]
        with self.assertRaises(ReleaseError):
            client.verify_annotated_tag(
                {"object": {"type": "tag", "sha": "c" * 40}},
                "v0.0.1",
                self.commit,
                "release",
            )
        client.http = FakeHttp(self.commit, "other")  # type: ignore[assignment]
        with self.assertRaisesRegex(ReleaseError, "different annotation"):
            client.verify_annotated_tag(
                {"object": {"type": "tag", "sha": "c" * 40}},
                "v0.0.1",
                self.commit,
                "release",
            )

    def test_existing_asset_mismatch_is_rejected(self) -> None:
        client = GitHubReleaseClient.__new__(GitHubReleaseClient)
        with self.assertRaises(ReleaseError):
            client.verify_asset(
                {"name": "x", "size": 1, "digest": "sha256:" + "0" * 64},
                self.checksum,
                1,
                "x",
            )

    def test_existing_release_is_idempotent_only_when_exact(self) -> None:
        required = {
            "tag_name": "v0.0.1",
            "name": "Smear 0.0.1",
            "body": "notes\n",
            "prerelease": False,
        }
        validate_release_fields({**required, "draft": True}, required)
        validate_release_fields({**required, "draft": False}, required)
        with self.assertRaises(ReleaseError):
            validate_release_fields({**required, "body": "changed\n"}, required)

    def test_draft_release_lookup_is_unique_and_draft_aware(self) -> None:
        draft = {"id": 1, "tag_name": "v0.0.1", "draft": True}
        published = {"id": 2, "tag_name": "v0.0.0", "draft": False}
        self.assertIs(select_release_by_tag([published, draft], "v0.0.1"), draft)
        self.assertIsNone(select_release_by_tag([published], "v0.0.1"))
        with self.assertRaisesRegex(ReleaseError, "multiple releases"):
            select_release_by_tag([draft, dict(draft)], "v0.0.1")

    def test_asset_index_rejects_extras_and_duplicates(self) -> None:
        wanted = {"a.crate", "SHA256SUMS"}
        indexed = index_release_assets(
            [{"name": "a.crate"}, {"name": "SHA256SUMS"}], wanted
        )
        self.assertEqual(set(indexed), wanted)
        with self.assertRaisesRegex(ReleaseError, "unexpected asset"):
            index_release_assets([{"name": "extra"}], wanted)
        with self.assertRaisesRegex(ReleaseError, "duplicate asset"):
            index_release_assets([{"name": "a.crate"}, {"name": "a.crate"}], wanted)


def cmd_selftest(_args: argparse.Namespace) -> None:
    suite = unittest.defaultTestLoader.loadTestsFromTestCase(ReleaseSelfTests)
    result = unittest.TextTestRunner(verbosity=2).run(suite)
    if not result.wasSuccessful():
        raise SystemExit(1)


def parser() -> argparse.ArgumentParser:
    root = argparse.ArgumentParser(description=__doc__)
    subparsers = root.add_subparsers(dest="command", required=True)

    selftest = subparsers.add_parser("selftest")
    selftest.set_defaults(func=cmd_selftest)

    def context_arguments(command: argparse.ArgumentParser) -> None:
        command.add_argument("--version", required=True)
        command.add_argument("--commit", required=True)
        command.add_argument("--target-dir", required=True)

    verify = subparsers.add_parser("verify")
    context_arguments(verify)
    verify.add_argument("--github-gates", action="store_true")
    verify.add_argument("--no-workspace", action="store_true")
    verify.set_defaults(func=cmd_verify)

    prepare = subparsers.add_parser("prepare")
    context_arguments(prepare)
    prepare.set_defaults(func=cmd_prepare)

    restore = subparsers.add_parser("restore")
    restore.add_argument("--lock-env", default="RELEASE_LOCK_B64")
    restore.add_argument("--lock-sha256", required=True)
    restore.add_argument("--manifest-env", default="RELEASE_PREFLIGHT_MANIFEST_B64")
    restore.add_argument("--manifest-sha256", required=True)
    restore.add_argument("--manifest-output", required=True)
    restore.set_defaults(func=cmd_restore)

    restore_json = subparsers.add_parser("restore-json")
    restore_json.add_argument("--encoded-env", required=True)
    restore_json.add_argument("--sha256", required=True)
    restore_json.add_argument("--label", required=True)
    restore_json.add_argument("--output", required=True)
    restore_json.set_defaults(func=cmd_restore_json)

    package = subparsers.add_parser("package")
    context_arguments(package)
    package.add_argument("--lock-sha256", required=True)
    package.add_argument("--expected-manifest", required=True)
    package.add_argument("--output")
    package.set_defaults(func=cmd_package)

    publish = subparsers.add_parser("publish")
    context_arguments(publish)
    publish.add_argument("--lock-sha256", required=True)
    publish.add_argument("--package-manifest", required=True)
    publish.add_argument("--poll-timeout", type=float, default=180)
    publish.add_argument("--output", required=True)
    publish.set_defaults(func=cmd_publish)

    release = subparsers.add_parser("release")
    context_arguments(release)
    release.add_argument("--lock-sha256", required=True)
    release.add_argument("--package-manifest", required=True)
    release.add_argument("--registry-manifest", required=True)
    release.add_argument("--poll-timeout", type=float, default=180)
    release.add_argument("--evidence-dir", required=True)
    release.set_defaults(func=cmd_release)

    return root


def main() -> int:
    args = parser().parse_args()
    try:
        args.func(args)
    except ReleaseError as error:
        if os.environ.get("GITHUB_ACTIONS"):
            print(f"::error::{error}", file=sys.stderr)
        else:
            print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
