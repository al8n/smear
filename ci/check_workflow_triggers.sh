#!/usr/bin/env bash
#
# Guards the one thing a workflow cannot check about itself: whether it runs at all.
#
# `on.push.branches` is a list of LITERAL branch names. It has to be — `on:` is resolved before any
# context exists, so `${{ github.event.repository.default_branch }}` and friends are unavailable
# there and no expression can stand in for the name. A literal list goes stale silently, and the
# only symptom is an absence: `gh run list --branch <trunk>` printing nothing. That is #74. All
# four workflows listed `main` alone while every pull request was merged into `feat/tokora-0.8`,
# so the trunk went its entire life without a single post-merge run and nothing anywhere said so.
#
# Nothing inside a run can report its own non-existence, so the check has to be an assertion made
# by the runs that DO happen. Four invariants:
#
#   1. Every workflow carrying an `on.push.branches` filter carries the SAME one. This repository
#      has already had a sibling-drift incident — `loc.yml` kept a `paths-ignore` entry `ci.yml`
#      had deliberately removed — and a post-merge lane that covers the trunk in `ci.yml` but not
#      in `miri.yml` is worse than one that covers it nowhere, because it looks fixed.
#
#   2. Every branch named still exists on the remote. This catches a typo and a deleted branch.
#      How much of the rename check it is depends on what becomes of the branch left behind, and
#      this repository has done it both ways. The first two trunks (`refactor/main`,
#      `refactor/main-0.8`) were abandoned in place, and for as long as a ref is left standing an
#      entry naming it resolves and this invariant never fires — invariant 4 is what catches that
#      case. `feat/tokora-0.8` was the first trunk retired by DELETING the ref instead of leaving
#      it, and against a deleted branch this invariant is immediate: it fails every workflow the
#      moment the ref goes. All three are gone from origin as of 2026-08-07, so the deletion ORDER
#      is what has to be got right, and it governs the temporary integration branches this list
#      also names, not just trunks: merge the entry's removal from every list BEFORE deleting the
#      ref. The reverse order reddens CI repository-wide, over a branch nobody is working on any
#      more.
#
#   3. The repository's default branch is in the list. This catches a RETARGET — the default
#      branch is what `schedule` runs and what a fresh clone lands on, so it needs a push lane
#      whether or not it is also the branch pull requests are merged into.
#
#   4. On a pull request, the BASE branch is in the list — unless that base is itself the head of
#      an open pull request, which is what a stacked PR looks like and where the verdict travels
#      onward with the parent. This is the one that catches a rename, because it does not ask what
#      the trunk is called; it asks where merges are actually going. Run against this repository
#      before #74 was fixed it fails on PR #76 (base `feat/tokora-0.8`, absent from every list),
#      which is the defect itself.
#
# Run by the `triggers` job in `.github/workflows/ci.yml`, and standalone from the repo root:
#
#     ci/check_workflow_triggers.sh
#
# Requires `yq` v4 and a remote named `origin`; invariant 4 additionally needs an authenticated
# `gh` and is skipped with a warning without one. Both are preinstalled on GitHub-hosted ubuntu
# runners. `DEFAULT_BRANCH` and `PR_BASE` may be supplied by the caller; CI passes
# `github.event.repository.default_branch` and `github.base_ref`, and a standalone run falls back
# to `refs/remotes/origin/HEAD` and to skipping invariant 4.

set -euo pipefail

fail=0

err() {
  if [ -n "${GITHUB_ACTIONS:-}" ]; then
    printf '::error::%s\n' "$*" >&2
  else
    printf 'error: %s\n' "$*" >&2
  fi
  fail=1
}

warn() {
  if [ -n "${GITHUB_ACTIONS:-}" ]; then
    printf '::warning::%s\n' "$*" >&2
  else
    printf 'warning: %s\n' "$*" >&2
  fi
}

if ! command -v yq >/dev/null 2>&1; then
  err "yq is not installed; cannot read the workflows' trigger blocks"
  exit 1
fi

cd "$(dirname "$0")/.."

shopt -s nullglob
files=(.github/workflows/*.yml .github/workflows/*.yaml)
shopt -u nullglob

if [ ${#files[@]} -eq 0 ]; then
  err "no workflow files found under .github/workflows"
  exit 1
fi

# ── 1. every push-filtered workflow agrees on the list ──────────────────────────────────────────

reference=""
reference_file=""
filtered=0

printf '%s\n' "on.push.branches, per workflow:"
for f in "${files[@]}"; do
  list=$(yq '.on.push.branches // [] | .[]' "$f")

  if [ -z "$list" ]; then
    printf '  %-34s (no push-branch filter, not checked)\n' "$f"
    continue
  fi

  filtered=$((filtered + 1))
  printf '  %-34s %s\n' "$f" "$(printf '%s' "$list" | tr '\n' ' ')"

  if [ -z "$reference_file" ]; then
    reference=$list
    reference_file=$f
  elif [ "$list" != "$reference" ]; then
    err "$f's push-branch list differs from $reference_file's. Every workflow with an \
on.push.branches filter must carry the same list, or a merge is covered by some lanes and not \
others."
  fi
done

if [ "$filtered" -eq 0 ]; then
  err "no workflow declares an on.push.branches filter, so no push to any branch runs anything"
  exit 1
fi

# ── 2. every name in the list still resolves ────────────────────────────────────────────────────

while IFS= read -r branch; do
  [ -n "$branch" ] || continue
  case "$branch" in
    *'*'* | *'['* | *'?'*)
      warn "'$branch' is a pattern, not a branch name — invariant 2 cannot check it"
      continue
      ;;
  esac

  set +e
  git ls-remote --heads --exit-code origin "refs/heads/$branch" >/dev/null 2>&1
  status=$?
  set -e

  case "$status" in
    0) ;;
    2) err "the push lane names '$branch', which does not exist on origin" ;;
    *) warn "could not reach origin to confirm '$branch' exists (git ls-remote exit $status)" ;;
  esac
done <<EOF
$reference
EOF

# ── 3. the default branch is covered ────────────────────────────────────────────────────────────

default_branch="${DEFAULT_BRANCH:-}"
if [ -z "$default_branch" ]; then
  default_branch=$(git symbolic-ref --quiet --short refs/remotes/origin/HEAD 2>/dev/null | sed 's#^origin/##' || true)
fi

if [ -z "$default_branch" ]; then
  warn "default branch unknown (set DEFAULT_BRANCH or configure refs/remotes/origin/HEAD); \
invariant 3 not checked"
elif ! printf '%s\n' "$reference" | grep -qxF "$default_branch"; then
  err "the repository's default branch '$default_branch' has no push lane. It is what schedule \
runs and what a fresh clone lands on; it needs one whether or not it is also the branch pull \
requests are merged into."
else
  printf 'default branch %s is covered\n' "$default_branch"
fi

# ── 4. this pull request's base is covered ──────────────────────────────────────────────────────

pr_base="${PR_BASE:-}"

if [ -z "$pr_base" ]; then
  printf 'not a pull request (PR_BASE unset); invariant 4 not applicable\n'
elif printf '%s\n' "$reference" | grep -qxF "$pr_base"; then
  printf 'pull request base %s is covered\n' "$pr_base"
elif ! command -v gh >/dev/null 2>&1; then
  warn "base branch '$pr_base' has no push lane, and gh is unavailable to tell a renamed trunk \
from a stacked pull request; invariant 4 not checked"
else
  set +e
  stacked=$(gh pr list --head "$pr_base" --state open --json number --jq 'length' 2>/dev/null)
  status=$?
  set -e

  if [ "$status" -ne 0 ]; then
    warn "base branch '$pr_base' has no push lane, and gh could not be queried (exit $status) to \
tell a renamed trunk from a stacked pull request; invariant 4 not checked"
  elif [ "$stacked" != "0" ]; then
    printf 'pull request base %s is the head of an open pull request (stacked); not required in \
the push lane\n' "$pr_base"
  else
    err "this pull request merges into '$pr_base', which no workflow has a push lane for and \
which is not itself the head of an open pull request. Nothing will run against the merge commit \
— add '$pr_base' to on.push.branches in EVERY workflow that has the filter. This is #74: the \
lists said 'main' while every merge went to the trunk, and the trunk never got a single run."
  fi
fi

if [ "$fail" -ne 0 ]; then
  exit 1
fi

printf '%s\n' "all $filtered push-filtered workflows agree, and every merge target is covered"
