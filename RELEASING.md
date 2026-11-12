# Releasing Smear

Smear releases these eight crates as one versioned set:

- `smear-lexer`
- `smear-parser`
- `smear-semantics`
- `smear-schema`
- `smear-compiler`
- `graphql-proto`
- `smear-analysis`
- `smear`

`smear-noatomic`, `smear-smoke`, and `source-census` are workspace checks, not
published packages. `ci/downstream` is a separately locked consumer fixture.

## Release preflight

Start from the exact release commit in a clean checkout. The eight publishable
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
release-bumped: `ci/downstream.sh` refreshes only a private snapshot's local
path-package versions before running the fixture's locked legs.

The workspace dry run is deliberately unlocked. During first publication,
Cargo's temporary registry cannot verify a root lock containing unpublished
path packages (Cargo 1.99 and 1.100 report a missing local checksum). The
existing unlocked workspace dry run still packages the eight crates in DAG order
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
smear-lexer
smear-parser      after smear-lexer
smear-semantics   no internal dependency
smear-schema      after smear-parser and smear-semantics
smear-compiler    after smear-parser and smear-schema
graphql-proto     after smear-parser and smear-schema
smear-analysis    after smear-lexer, smear-parser and smear-semantics
smear             after every crate above
```

`smear-compiler` and `graphql-proto` may proceed after `smear-schema`, and
`smear-analysis` after `smear-parser` and `smear-semantics`; publish `smear`
only after all seven are visible. For every package, compare the checksum in
the crates.io index with the locally recorded `.crate` checksum before moving to
the dependent package.

## Bootstrap and trusted publishing

`0.0.1` has one manual bootstrap exception: the seven sibling crates, published
by `al8n`. The existing `smear` name needs no bootstrap. **Never manually publish
`smear 0.0.1`: the controller requires GitHub OIDC provenance for the facade.**
A missing crate name cannot be created through trusted publishing.

Use one release run for both the manual bootstrap and OIDC publication:

1. Merge the reviewed release changes and wait for CI, Windows, macOS, and Miri
   push runs to succeed on that exact main SHA. Dispatch `release.yml` from main with that
   full SHA and version `0.0.1`.
2. Let `verify` finish, then leave the `crates-io` publish job waiting for
   approval. `verify` generates the ignored root lock exactly once and exports
   its existing bytes, preflight manifest, and eight archives as an immutable
   `smear-preflight-VERSION-COMMIT-RUN-ATTEMPT` artifact. Its summary records the
   artifact ID/digest and the lock, manifest, and archive SHA-256 values.
3. Download that exact artifact ID. Verify its repository, workflow, event
   (`workflow_dispatch`), main-branch SHA, producing attempt, and successful
   `verify` job. The whole run is still waiting, not completed. Compare the
   downloaded artifact ZIP's SHA-256 with the upload step's digest before
   extracting it. Take the lock and manifest hashes from the trusted run
   summary, not merely from the downloaded `SHA256SUMS`.
4. Check out that same SHA in a clean, private checkout. Keep the downloaded
   bundle, Cargo target directory, and bootstrap evidence outside the checkout.
   Use Cargo `1.99.1` and the existing authenticated `gh` session for gate reads.
   Run the restricted command below. It restores the downloaded lock before
   Cargo, reproduces all eight archives, and refuses any byte difference before
   uploading the seven siblings. It never generates another lock or selects the
   facade.
5. After all seven siblings are verified, immediately revoke and unset the
   bootstrap credential. Configure all eight crates' trusted publishers through
   the crates.io website: owner `al8n`, repository `smear`, workflow `release.yml`,
   environment `crates-io`. Enable Trusted-Publishing-only mode for all eight.
6. Approve the **same waiting run**. Its protected job restores the original
   lock from `needs.verify.outputs`, matches the original archives, proves and
   skips the seven manual bootstrap versions, and publishes `smear` via OIDC.
   Only afterward does the separate GitHub job create the tag and release.

For the manual step, create the shortest-lived scoped credential under `al8n`,
restricted to these seven exact crate scopes: `smear-lexer`, `smear-parser`,
`smear-semantics`, `smear-schema`, `smear-compiler`, `graphql-proto`,
`smear-analysis`. Grant only `publish-new` and `trusted_publishing` endpoint
permissions. The latter is needed solely for an authenticated identity read:
crates.io's `/me` route is cookie-only, while
`GET /api/v1/trusted_publishing/github_configs?user_id=...` verifies that the
credential belongs to the public `al8n` identity. The command only reads that
endpoint; it never creates, changes, or deletes a publisher configuration.
If that identity cannot be proven, it stops before uploads. Do not include
`smear` in the credential's crate scopes.

Pass the credential only as the local process environment variable
`CARGO_REGISTRY_TOKEN`. Never put it in chat, arguments, a temporary file,
`cargo login`, Git, or GitHub secrets. Do not enable shell tracing. All values
in the following commands are nonsecret evidence from the reviewed run; use a
private scratch directory and an external Cargo target directory:

```sh
gh api "repos/al8n/smear/actions/artifacts/$ARTIFACT_ID" > "$SCRATCH/artifact.json"
gh api "repos/al8n/smear/actions/artifacts/$ARTIFACT_ID/zip" > "$SCRATCH/preflight.zip"
shasum -a 256 "$SCRATCH/preflight.zip"
# Compare against the trusted run's artifact digest, then extract:
unzip -q "$SCRATCH/preflight.zip" -d "$SCRATCH/preflight"

python3 ci/release.py bootstrap \
  --version 0.0.1 --commit "$RELEASE_COMMIT" \
  --bundle-dir "$SCRATCH/preflight" \
  --lock-sha256 "$LOCK_SHA256" --manifest-sha256 "$MANIFEST_SHA256" \
  --target-dir "$CARGO_TARGET_DIR" --output "$SCRATCH/bootstrap-registry.json"
```

The root `Cargo.lock` remains ignored and untracked. Keep this exact lock and
the downloaded bundle with the release evidence; never commit it. An existing
local lock is accepted only if it is byte-identical to the frozen bundle.

## Recovery and provenance

An upload can succeed before Cargo observes the new index entry. On a timeout
or any ambiguous result, do not retry blindly: query crates.io and the index,
download the published archive if present, and compare its checksum, package
contents, and `.cargo_vcs_info.json` with the preflight record. Only then
continue the DAG. Published versions are immutable; a bad release requires a
new version (and, where appropriate, a yank), never an overwrite.

Resume a partial manual bootstrap with the same bundle, hashes, checkout SHA,
and restricted command. It proves all existing sibling versions before adding
any uploads, skips exact matches, and saves evidence after every verified
package. A mismatch is fatal. Retain the original run, producing attempt,
artifact ID/digest, lock, manifest, and partial registry evidence.

Rerun only failed `publish` or `release` jobs in that original workflow run.
Never rerun `verify` or start a fresh `0.0.1` preflight after any sibling has
been published: preparation refuses both reruns and existing sibling versions
rather than silently resolving another lock. Before bootstrap begins, a failed
preflight may be replaced by a new run, discarding the unused evidence.
GitHub permits job reruns for 30 days; artifact retention is 90 days. Outside
the rerun window, stop for a separately reviewed recovery procedure using the
retained frozen evidence. There is no automatic fresh-resolution fallback.

For an OIDC release, retain the tag, commit SHA, workflow run URL and identity,
generated-lock checksum, local and registry archive checksums, package README
comparison, and VCS-info comparison. This is the provenance record for the
release.
