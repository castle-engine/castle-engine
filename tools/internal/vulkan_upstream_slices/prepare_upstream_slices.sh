#!/usr/bin/env bash
# ------------------------------------------------------------------------------
# prepare_upstream_slices.sh
#
# Deterministic, non-destructive verifier, summary reporter, and patch-series
# exporter for splitting the Castle Game Engine Vulkan 1.3+ RHI branch into
# 8 dependency-ordered, acyclic upstream PR slices against
# castle-engine/castle-engine.
#
# Safety guarantees:
#   - NEVER modifies tracked repository files, git index, or branches.
#   - NEVER pushes to any remote or opens any pull request.
#   - Compatible with bash 3.2+ (Linux, macOS Apple Silicon, Windows Git Bash)
#     and both full git clones and shallow CI checkouts (actions/checkout).
# ------------------------------------------------------------------------------
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
MANIFEST_FILE="${SCRIPT_DIR}/slice_manifest.tsv"
DEFAULT_MERGE_BASE="9ba97754ec971ace540a7b478ebe61f974cc860b"

DO_VERIFY=0
DO_SUMMARY=0
EXPORT_DIR=""
EXPLICIT_BASE=""

usage() {
  cat <<'EOF'
Usage: prepare_upstream_slices.sh [options]

Options:
  --verify                 Verify slice_manifest.tsv integrity, file existence,
                           zero duplicates, line-ending hygiene, acyclic Pascal
                           uses-clause slice ordering, and (when base commit is
                           present in git history) 100% git diff partition coverage.
  --summary                Print per-slice file breakdown table (Slices 1..8 + 0).
  --base=<commit-or-ref>   Explicit base commit/ref for git diff verification
                           and patch export (default: auto-detect upstream/master
                           merge-base or e97e84b7aa51f628a7978c5420cefb53a3da05ab).
  --export-patches=<dir>   Export clean per-slice .patch files (Slices 1..8, with
                           Slice 3 split into core vs 03b-examples-auto-dproj)
                           into <dir> without modifying the working tree.
  -h, --help               Show this help message.
EOF
}

for arg in "$@"; do
  case "$arg" in
    --verify)
      DO_VERIFY=1
      ;;
    --summary)
      DO_SUMMARY=1
      ;;
    --base=*)
      EXPLICIT_BASE="${arg#*=}"
      ;;
    --export-patches=*)
      EXPORT_DIR="${arg#*=}"
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "ERROR: Unknown option: $arg" >&2
      usage >&2
      exit 1
      ;;
  esac
done

if [ "$DO_VERIFY" -eq 0 ] && [ "$DO_SUMMARY" -eq 0 ] && [ -z "$EXPORT_DIR" ]; then
  DO_VERIFY=1
  DO_SUMMARY=1
fi

if [ ! -f "$MANIFEST_FILE" ]; then
  echo "ERROR: Missing slice manifest: $MANIFEST_FILE" >&2
  exit 1
fi

cd "$REPO_ROOT"

resolve_base_commit() {
  if [ -n "$EXPLICIT_BASE" ]; then
    if git rev-parse --verify --quiet "${EXPLICIT_BASE}^{commit}" >/dev/null 2>&1; then
      git rev-parse "${EXPLICIT_BASE}^{commit}"
      return 0
    fi
    echo "ERROR: Explicit --base='${EXPLICIT_BASE}' is not reachable in local git history." >&2
    return 1
  fi

  if git rev-parse --verify --quiet "upstream/master^{commit}" >/dev/null 2>&1; then
    git merge-base upstream/master HEAD
    return 0
  fi

  if git rev-parse --verify --quiet "${DEFAULT_MERGE_BASE}^{commit}" >/dev/null 2>&1; then
    echo "$DEFAULT_MERGE_BASE"
    return 0
  fi

  echo ""
  return 0
}

BASE_COMMIT="$(resolve_base_commit)"

# ------------------------------------------------------------------------------
# 1. Manifest Integrity, Disk Existence, Line-Ending Hygiene & Pascal uses DAG
# ------------------------------------------------------------------------------
if [ "$DO_VERIFY" -eq 1 ]; then
  echo "[vulkan-slices] Verifying manifest structure and repository files..."

  # Check LF hygiene on shell script itself (guaranteed via .gitattributes *.sh text eol=lf)
  if LC_ALL=C grep -q $'\r' "${SCRIPT_DIR}/prepare_upstream_slices.sh"; then
    echo "ERROR: prepare_upstream_slices.sh contains CRLF line endings (expected pure LF)." >&2
    exit 1
  fi

  awk -F '\t' '
    BEGIN {
      errors = 0
      total = 0
    }
    { sub(/\r$/, "") }
    /^[[:space:]]*#/ || /^[[:space:]]*$/ { next }
    {
      if (NF != 4) {
        printf("ERROR: Line %d: expected 4 tab-separated columns, got %d\n", NR, NF) > "/dev/stderr"
        errors++
        next
      }
      sid = $1
      slug = $2
      cat = $3
      path = $4

      if (sid !~ /^[0-8]$/) {
        printf("ERROR: Line %d: invalid slice_id \"%s\" for %s\n", NR, sid, path) > "/dev/stderr"
        errors++
      }
      if (cat != "core" && cat != "auto-dproj" && cat != "branch-meta") {
        printf("ERROR: Line %d: invalid category \"%s\" for %s\n", NR, cat, path) > "/dev/stderr"
        errors++
      }
      if (sid == "0" && cat != "branch-meta") {
        printf("ERROR: Line %d: slice 0 entry %s must have category branch-meta\n", NR, path) > "/dev/stderr"
        errors++
      }
      if (sid != "0" && cat == "branch-meta") {
        printf("ERROR: Line %d: non-zero slice %s entry %s cannot be branch-meta\n", NR, sid, path) > "/dev/stderr"
        errors++
      }
      if (path in seen) {
        printf("ERROR: Duplicate path \"%s\" on line %d (first seen on line %d)\n", path, NR, seen[path]) > "/dev/stderr"
        errors++
      }
      seen[path] = NR
      slice_counts[sid]++
      total++
    }
    END {
      for (s = 1; s <= 8; s++) {
        if (slice_counts[s] + 0 == 0) {
          printf("ERROR: Slice %d has 0 assigned files\n", s) > "/dev/stderr"
          errors++
        }
      }
      if (errors > 0) exit 1
      printf("[vulkan-slices] Manifest syntax OK (%d unique tracked entries across Slices 0..8).\n", total)
    }
  ' "$MANIFEST_FILE"

  # Verify every listed upstream slice path (Slices 1..8) exists on disk
  MISSING_ON_DISK=0
  while IFS=$'\t' read -r sid slug cat relpath; do
    relpath="${relpath%$'\r'}"
    case "$sid" in
      ""|\#*|0) continue ;;
    esac
    if [ ! -e "$relpath" ]; then
      echo "ERROR: Manifest file missing on disk: $relpath (slice $sid)" >&2
      MISSING_ON_DISK=$((MISSING_ON_DISK + 1))
    fi
  done < "$MANIFEST_FILE"

  if [ "$MISSING_ON_DISK" -ne 0 ]; then
    echo "ERROR: $MISSING_ON_DISK manifest file(s) missing on disk." >&2
    exit 1
  fi

  # Verify required documentation & release notes exist and are non-empty
  for doc_file in \
    "doc/vulkan_renderer.md" \
    "doc/vulkan_release_notes.md" \
    "doc/vulkan_upstream_pr_guide.md"; do
    if [ ! -s "$doc_file" ]; then
      echo "ERROR: Required documentation file missing or empty: $doc_file" >&2
      exit 1
    fi
  done

  # Verify strict acyclic Pascal uses-clause ordering across Slices 1..8:
  # No .pas unit in Slice S (1..8) may list in its uses clause any unit whose .pas
  # file is assigned to a strictly later Slice T > S!
  # Uses O(N) line-by-line streaming so BSD one-true-awk (macOS), mawk, and GNU gawk
  # all process multi-megabyte .pas files in < 50 ms without quadratic string copies.
  echo "[vulkan-slices] Verifying acyclic Pascal uses-clause DAG across Slices 1..8..."
  awk -F '\t' '
    function basename_no_ext(p,    n, arr, base) {
      n = split(p, arr, "/")
      base = tolower(arr[n])
      sub(/\.pas$/, "", base)
      return base
    }
    function dirname_of(p,    idx) {
      idx = match(p, /\/[^\/]*$/)
      if (idx > 0) return substr(p, 1, idx - 1)
      return "."
    }
    function check_uses_clause(src_path, s_from, clause,    n, k, items, tok, s_to) {
      n = split(clause, items, ",")
      for (k = 1; k <= n; k++) {
        tok = items[k]
        sub(/[[:space:]]+in[[:space:]]+.*$/, "", tok)
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", tok)
        if (tok != "" && (tok in unit_slice)) {
          s_to = unit_slice[tok]
          if (s_to > s_from) {
            printf("ERROR: Forward slice dependency violation: %s (Slice %d) uses %s (%s in Slice %d)\n",
              src_path, s_from, tok, unit_path[tok], s_to) > "/dev/stderr"
            violations++
          }
        }
      }
    }
    function scan_stream(root_pas, fpath, dir, s_from, depth,    line, inc_tag, inc_cand, probe, pos_open, pos_close, pre, post, semi_pos) {
      if (depth > 4) return
      while ((getline line < fpath) > 0) {
        sub(/\r$/, "", line)

        # Expand local {$I foo.inc} / {$INCLUDE foo.inc} directives when outside block comments
        if (cstate == 0 && match(line, /\{\$[iI]([nN][cC][lL][uU][dD][eE])?[[:space:]]+[^}[:space:]]+[[:space:]]*\}/)) {
          inc_tag = substr(line, RSTART, RLENGTH)
          sub(/^\{\$[iI]([nN][cC][lL][uU][dD][eE])?[[:space:]]+/, "", inc_tag)
          sub(/[[:space:]]*\}$/, "", inc_tag)
          if (tolower(inc_tag) !~ /castleconf\.inc$/) {
            inc_cand = dir "/" inc_tag
            if ((getline probe < inc_cand) > 0) {
              close(inc_cand)
              scan_stream(root_pas, inc_cand, dir, s_from, depth + 1)
            } else {
              close(inc_cand)
              inc_cand = dir "/" tolower(inc_tag)
              if ((getline probe < inc_cand) > 0) {
                close(inc_cand)
                scan_stream(root_pas, inc_cand, dir, s_from, depth + 1)
              } else {
                close(inc_cand)
              }
            }
          }
        }

        # Strip single-quoted Pascal string literals on this line before comment stripping
        if (cstate == 0 && index(line, apos) > 0) {
          gsub(apos "([^" apos "]|" apos apos ")*" apos, " ", line)
        }

        # Strip multi-line { ... } and (* ... *) comments across lines
        while (line != "") {
          if (cstate == 1) {
            pos_close = index(line, "}")
            if (pos_close == 0) { line = ""; break }
            line = substr(line, pos_close + 1)
            cstate = 0
          } else if (cstate == 2) {
            pos_close = index(line, "*)")
            if (pos_close == 0) { line = ""; break }
            line = substr(line, pos_close + 2)
            cstate = 0
          } else {
            # Remove // line comment if it precedes any { or (*
            pos_open = index(line, "{")
            pos_close = index(line, "(*")
            if (match(line, /\/\//)) {
              if ((pos_open == 0 || RSTART < pos_open) && (pos_close == 0 || RSTART < pos_close)) {
                line = substr(line, 1, RSTART - 1)
                pos_open = index(line, "{")
                pos_close = index(line, "(*")
              }
            }
            if (pos_open > 0 && (pos_close == 0 || pos_open < pos_close)) {
              pre = substr(line, 1, pos_open - 1)
              post = substr(line, pos_open + 1)
              pos_close = index(post, "}")
              if (pos_close > 0) {
                line = pre " " substr(post, pos_close + 1)
              } else {
                line = pre
                cstate = 1
                break
              }
            } else if (pos_close > 0) {
              pre = substr(line, 1, pos_close - 1)
              post = substr(line, pos_close + 2)
              pos_open = index(post, "*)")
              if (pos_open > 0) {
                line = pre " " substr(post, pos_open + 2)
              } else {
                line = pre
                cstate = 2
                break
              }
            } else {
              break
            }
          }
        }

        if (line == "") continue
        line = tolower(line)

        # Accumulate only active uses ... ; clauses
        while (line != "") {
          if (in_uses == 1) {
            semi_pos = index(line, ";")
            if (semi_pos > 0) {
              uses_buf = uses_buf " " substr(line, 1, semi_pos - 1)
              check_uses_clause(root_pas, s_from, uses_buf)
              uses_buf = ""
              in_uses = 0
              line = substr(line, semi_pos + 1)
            } else {
              uses_buf = uses_buf " " line
              line = ""
            }
          } else {
            if (match(line, /(^|[^a-z0-9_.])uses($|[^a-z0-9_])/)) {
              line = substr(line, RSTART + RLENGTH - 1)
              sub(/^uses/, "", line)
              in_uses = 1
              uses_buf = ""
            } else {
              line = ""
            }
          }
        }
      }
      close(fpath)
    }
    { sub(/\r$/, "") }
    /^[[:space:]]*#/ || /^[[:space:]]*$/ { next }
    {
      sid = $1 + 0
      path = $4
      if (sid >= 1 && path ~ /\.pas$/ && (path ~ /^src\// || path ~ /^tools\// || path ~ /^tests\//)) {
        u = basename_no_ext(path)
        unit_slice[u] = sid
        unit_path[u] = path
        pas_files[++pas_count] = path
        pas_slice[pas_count] = sid
      }
    }
    END {
      apos = sprintf("%c", 39)
      violations = 0
      for (idx = 1; idx <= pas_count; idx++) {
        cstate = 0
        in_uses = 0
        uses_buf = ""
        scan_stream(pas_files[idx], pas_files[idx], dirname_of(pas_files[idx]), pas_slice[idx], 0)
      }
      if (violations > 0) exit 1
      printf("[vulkan-slices] Pascal uses-clause DAG OK (%d .pas units checked, 0 forward cross-slice dependencies).\n", pas_count)
    }
  ' "$MANIFEST_FILE"

  # Full git diff partition verification when BASE_COMMIT is available
  if [ -n "$BASE_COMMIT" ]; then
    echo "[vulkan-slices] Verifying 100% git diff coverage against base commit ${BASE_COMMIT}..."
    TMP_DIFF="$(mktemp)"
    TMP_MAN="$(mktemp)"
    trap 'rm -f "$TMP_DIFF" "$TMP_MAN"' EXIT

    # Combine committed changes since BASE_COMMIT plus any staged/unstaged/untracked manifest files
    {
      git diff --name-only "${BASE_COMMIT}..HEAD"
      git diff --name-only
      git diff --cached --name-only
      git ls-files --others --exclude-standard
    } | awk 'NF > 0' | LC_ALL=C sort -u > "$TMP_DIFF"

    awk -F '\t' '{ sub(/\r$/, "") } !/^[[:space:]]*#/ && NF == 4 { print $4 }' "$MANIFEST_FILE" | LC_ALL=C sort -u > "$TMP_MAN"
    TMP_MAN_UPSTREAM="$(mktemp)"
    awk -F '\t' '{ sub(/\r$/, "") } !/^[[:space:]]*#/ && NF == 4 && $1 != "0" { print $4 }' "$MANIFEST_FILE" | LC_ALL=C sort -u > "$TMP_MAN_UPSTREAM"

    UNASSIGNED="$(LC_ALL=C comm -23 "$TMP_DIFF" "$TMP_MAN" || true)"
    NOT_IN_DIFF="$(LC_ALL=C comm -13 "$TMP_DIFF" "$TMP_MAN_UPSTREAM" || true)"
    rm -f "$TMP_MAN_UPSTREAM"

    if [ -n "$UNASSIGNED" ]; then
      echo "ERROR: Changed files missing from slice_manifest.tsv:" >&2
      echo "$UNASSIGNED" | sed 's/^/  - /' >&2
      exit 1
    fi
    if [ -n "$NOT_IN_DIFF" ]; then
      echo "ERROR: Entries in slice_manifest.tsv that are unchanged vs ${BASE_COMMIT}:" >&2
      echo "$NOT_IN_DIFF" | sed 's/^/  - /' >&2
      exit 1
    fi
    DIFF_COUNT="$(wc -l < "$TMP_DIFF" | tr -d ' ')"
    echo "[vulkan-slices] Git diff partition OK: 100% exact 1-to-1 coverage (${DIFF_COUNT}/${DIFF_COUNT} changed files)."
  else
    echo "[vulkan-slices] Note: Shallow git clone detected (merge-base commit not in local history); skipped git diff set comparison."
  fi
fi

# ------------------------------------------------------------------------------
# 2. Summary Table
# ------------------------------------------------------------------------------
if [ "$DO_SUMMARY" -eq 1 ]; then
  echo ""
  echo "================================================================================"
  echo " Castle Game Engine — Vulkan 1.3+ Upstream PR Slices Summary"
  echo "================================================================================"
  awk -F '\t' '
    { sub(/\r$/, "") }
    /^[[:space:]]*#/ || /^[[:space:]]*$/ { next }
    {
      sid = $1 + 0
      slug[sid] = $2
      cat = $3
      total[sid]++
      if (cat == "auto-dproj") auto_dproj[sid]++
      else core[sid]++
      grand_total++
      if (sid >= 1) upstream_total++
      if (sid >= 1 && cat != "auto-dproj") upstream_core++
    }
    END {
      printf(" %-7s %-44s %8s %10s %8s\n", "Slice", "PR Slug", "Core", "AutoDproj", "Total")
      printf(" ------------------------------------------------------------------------------\n")
      for (s = 1; s <= 8; s++) {
        printf(" PR #%-3d %-44s %8d %10d %8d\n",
          s, slug[s], core[s] + 0, auto_dproj[s] + 0, total[s] + 0)
      }
      printf(" ------------------------------------------------------------------------------\n")
      printf(" %-7s %-44s %8d %10d %8d\n",
        "1..8", "Total Upstream PR Scope", upstream_core + 0, auto_dproj[3] + 0, upstream_total + 0)
      if (total[0] + 0 > 0) {
        printf(" %-7s %-44s %8d %10d %8d\n",
          "Meta(0)", slug[0] " (branch-only)", core[0] + 0, 0, total[0] + 0)
      }
      printf(" %-7s %-44s %8s %10s %8d\n", "ALL", "Repository Diff Coverage", "-", "-", grand_total + 0)
    }
  ' "$MANIFEST_FILE"
  echo "================================================================================"
fi

# ------------------------------------------------------------------------------
# 3. Optional Read-Only Patch Series Export
# ------------------------------------------------------------------------------
if [ -n "$EXPORT_DIR" ]; then
  if [ -z "$BASE_COMMIT" ]; then
    echo "ERROR: --export-patches requires full git history reaching the merge-base commit (pass --base=<commit> or fetch upstream)." >&2
    exit 1
  fi

  mkdir -p "$EXPORT_DIR"
  SERIES_FILE="${EXPORT_DIR}/SERIES.txt"
  : > "$SERIES_FILE"

  for s in 1 2 3 4 5 6 7 8; do
    SLUG="$(awk -F '\t' -v target="$s" '{ sub(/\r$/, "") } $1 == target { print $2; exit }' "$MANIFEST_FILE")"
    PATCH_NAME="$(printf '%02d-%s.patch' "$s" "$SLUG")"
    PATCH_PATH="${EXPORT_DIR}/${PATCH_NAME}"

    MAP_FILES="$(mktemp)"
    awk -F '\t' -v target="$s" '{ sub(/\r$/, "") } $1 == target && $3 != "auto-dproj" { print $4 }' "$MANIFEST_FILE" > "$MAP_FILES"

    # Export committed + working-tree diff against BASE_COMMIT for core files in slice s
    git diff --binary "${BASE_COMMIT}" -- $(cat "$MAP_FILES") > "$PATCH_PATH"
    rm -f "$MAP_FILES"
    echo "$PATCH_NAME" >> "$SERIES_FILE"
    echo "[vulkan-slices] Wrote ${PATCH_PATH}"

    if [ "$s" -eq 3 ]; then
      AUTO_PATCH_NAME="03b-examples-auto-dproj.patch"
      AUTO_PATCH_PATH="${EXPORT_DIR}/${AUTO_PATCH_NAME}"
      AUTO_FILES="$(mktemp)"
      awk -F '\t' '{ sub(/\r$/, "") } $1 == "3" && $3 == "auto-dproj" { print $4 }' "$MANIFEST_FILE" > "$AUTO_FILES"
      if [ -s "$AUTO_FILES" ]; then
        git diff --binary "${BASE_COMMIT}" -- $(cat "$AUTO_FILES") > "$AUTO_PATCH_PATH"
        echo "$AUTO_PATCH_NAME" >> "$SERIES_FILE"
        echo "[vulkan-slices] Wrote ${AUTO_PATCH_PATH}"
      fi
      rm -f "$AUTO_FILES"
    fi
  done

  echo "[vulkan-slices] Exported upstream patch series (base ${BASE_COMMIT}) -> ${EXPORT_DIR}"
fi
