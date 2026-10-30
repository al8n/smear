# Releasing Smear

Smear releases these six crates as one versioned set:

- `smear-lexer`
- `smear-parser`
- `smear-schema`
- `smear-compiler`
- `graphql-proto`
- `smear`

`smear-noatomic`, `smear-smoke`, and `source-census` are workspace checks, not
published packages. `ci/downstream` is a separately locked consumer fixture.

## Release preflight

Start from the exact release commit in a clean checkout. The six publishable
packages must have the release version, and each internal package requirement
must be the matching caret requirement. Run the repository checks, then:

```sh
cargo generate-lockfile
shasum -a 256 Cargo.lock
cargo publish --dry-run --workspace
find "${CARGO_TARGET_DIR:-target}/package" -name "*.crate" -type f -exec shasum -a 256 {} \;
```

The root `Cargo.lock` is deliberately ignored. Record its SHA-256 with the
release evidence and remove it afterward; do not add it to Git. The committed
`ci/downstream/Cargo.lock` is a historical consumer-fixture input and is not
release-bumped: `ci/downstream.sh` refreshes only a private snapshot's six
local path-package versions before running the fixture's locked legs.

The workspace dry run is deliberately unlocked. During first publication,
Cargo's temporary registry cannot verify a root lock containing unpublished
path packages (Cargo 1.99 and 1.100 report a missing local checksum). The
existing unlocked workspace dry run still packages the six crates in DAG order
and verifies their packaged sources.

## Locked publication after approval

The protected release workflow must separately generate the root lock before
approval, hash it, and transfer the lock plus its SHA-256 as the approval
record. After approval, it reconstructs that exact lock in the protected
environment and verifies the digest before use. It then runs per-crate
`cargo package --locked` and `cargo publish --locked` in the DAG below, after
each dependency is visible in the registry. Do not regenerate the lock after
approval.

Inspect each package before publishing. Its packaged README must equal the
source README (the umbrella README is flattened from `smear/README.md`), and
its `.cargo_vcs_info.json` must name the clean release commit. Record the
annotated tag's commit, the generated-lock checksum, every `.crate` checksum,
and the package list with the release evidence.

## Publish order

Cargo replaces path dependencies with version requirements in a published
manifest, so publish in dependency order and verify registry visibility after
each step:

```text
smear-lexer -> smear-parser -> smear-schema
                                  |        \
                                  v         v
                            smear-compiler  graphql-proto
                                  \         /
                                   v       v
                                    smear
```

`smear-compiler` and `graphql-proto` may proceed after `smear-schema`; publish
`smear` only after both are visible. For every package, compare the checksum in
the crates.io index with the locally recorded `.crate` checksum before moving to
the dependent package.

## Bootstrap and trusted publishing

`0.0.1` is the one-time bootstrap exception. A crate must exist before its
crates.io trusted publisher can be configured, so bootstrap each first version
manually with a short-lived, least-privilege publish token. Pass that token only
through the process environment, never commit it, save it with `cargo login`,
or place it in GitHub secrets. Revoke it immediately after all six packages are
verified.

After bootstrap, configure a crates.io trusted publisher for every publishable
crate, bound to this repository, the reviewed release workflow, and its
protected release environment. Future releases use GitHub Actions OIDC with
`id-token: write`; the workflow exchanges that identity for a short-lived
crates.io publishing credential and holds no long-lived registry token. Keep
the workflow's package order equal to the DAG above.

## Recovery and provenance

An upload can succeed before Cargo observes the new index entry. On a timeout
or any ambiguous result, do not retry blindly: query crates.io and the index,
download the published archive if present, and compare its checksum, package
contents, and `.cargo_vcs_info.json` with the preflight record. Only then
continue the DAG. Published versions are immutable; a bad release requires a
new version (and, where appropriate, a yank), never an overwrite.

For an OIDC release, retain the tag, commit SHA, workflow run URL and identity,
generated-lock checksum, local and registry archive checksums, package README
comparison, and VCS-info comparison. This is the provenance record for the
release.
