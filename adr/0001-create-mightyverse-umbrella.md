# Create `mightyverse` umbrella package

**Status:** proposed (draft for team discussion)

**Author:** [your name] · **Date:** 2026-06-10

## Context

The `mighty` repo currently houses three things: (1) the orchestration engine itself, (2) ecosystem-wide vignettes that span `mighty`, `mighty.metadata`, and `mighty.component`, and (3) an integration-heavy test suite (47 test files; the suite is light on unit tests). This conflates concerns: a user landing on `mighty`'s pkgdown site reads docs that aren't really about `mighty`, and a CI failure in `mighty` may reflect breakage in a sibling package.

This ADR proposes creating a new R package, `mightyverse`, as a tidyverse-style umbrella that owns the ecosystem-level documentation and integration test suite. After migration, each `mighty.*` repo's documentation focuses on what that package alone does, and ecosystem-spanning concerns live in one place.

## Shape of the proposal *(discussion point)*

Two ways to split ecosystem concerns out of `mighty`:

**Option 1: create `mightyverse` as a new umbrella package.** Ecosystem docs and integration tests move out of `mighty` into the new package. `mighty` keeps its name, its 2 exports, and all its source code.

**Option 2: keep `mighty` as the user-facing pkg; move the engine into a new `mighty.something` package.** `mighty` becomes the umbrella, the landing page, and the home of integration tests. `mighty.something` holds the orchestration code and unit tests. The 2 exports are re-exported from the engine through `mighty`.

Option 2 requires a bit more work to ensure git history moves cleanly.



## Recommendations

The recommendations below assume Option 1. They are drafts for team discussion, not settled decisions.

### 1. Identity: umbrella package + ecosystem docs + integration tests

`mightyverse` serves three purposes simultaneously: an end-user umbrella package (à la `library(tidyverse)`), the home of ecosystem narrative documentation, and the home of cross-package integration tests.

*Alternatives considered:* a pure test/doc harness with no end-user install, or a user-facing umbrella with tests living elsewhere. 

### 2. What triggers the integration test runs *(discussion point)*

Recommendation: each sibling repo's CI runs `mightyverse`'s integration suite, in addition to its own unit tests, on PRs to main. This catches ecosystem breakage at PR time on the originating repo.

The cons: every sibling's CI now depends on `mightyverse`, and `mightyverse`'s `main` must stay green or it blocks the rest of the ecosystem.

*Alternatives:*
- Run integration tests only in `mightyverse` CI; rely on manual triggers or post-merge runs to catch cross-repo regressions. Not automatic, may allowing breaking changes to go un-addressed.
- Scheduled: nightly CI run `mightyverse` PRs. No real gain from manual, just a delay

### 3. Test cut line and unit-test backfill

**Cut line:** a test moves to `mightyverse` only if it meaningfully exercises behavior that emerges from multiple `mighty.*` packages cooperating. Tests that stress `mighty`'s own internals  `mighty`. If such a test fails, the bug is in `mighty`.

**Backfill *(discussion point)*:** migrate first, accept that `mighty`'s test coverage will visibly drop, file follow-up issues per uncovered area.

*Alternatives for backfill:*
- Block the migration on writing unit tests for `mighty` first. Cleanest end state, indeterminate timeline.
- Leave the integration tests duplicated in both repos and deduplicate later. Avoids the coverage gap but creates two suites that drift apart.

### 4. Vignettes: all narrative docs move

All ecosystem narrative vignettes (`mighty.qmd`, `adam_specification.qmd`, `connect_to_data.qmd`, `inputs_overview.qmd`, `mighty_config.qmd`, `code_components.qmd`, `special_components.qmd`, `missing_data_analysis.qmd`) move to `mightyverse`. `mighty` keeps `dev_documentation.qmd` and its function reference. `mighty.metadata` and `mighty.component` similarly become function-reference-only sites. Each sibling's pkgdown landing page redirects to `mightyverse` for narrative documentation. This is the dplyr-vs-tidyverse pattern.

*Alternative considered:* keep stub vignettes in siblings that link to `mightyverse`. Rejected because no use-case identified and maintenance overhead.

### 5. Attach behavior: tidyverse-style

`library(mightyverse)` attaches the sibling packages to the search path without re-exporting their symbols from its own namespace, matching `library(tidyverse)`. `mightyverse`'s `NAMESPACE` is sparse by design.

*Alternative considered:* re-export selected functions from siblings. Rejected because it creates a maintenance burden where every new public function in a sibling needs a corresponding `mightyverse` update.

### 6. Attach list: 3 packages

`mightyverse` attaches `mighty`, `mighty.metadata`, and `mighty.component`. `mighty.toolbox` is excluded because it is not currently open source. 

The attach list is reviewable when `mighty.toolbox` opens or if `mighty.standards` becomes a published R package with exports.

### 7. Documentation scope: OS pieces only

`mightyverse` documents the OS pipeline (YAML → action graph → generated R programs). Submission deliverables (`mighty.toolbox`) are not documented in `mightyverse`; toolbox docs stay in toolbox. If toolbox opens in the future, a stub vignette in `mightyverse` linking out becomes a reasonable option.

### 9. Migration sequencing: copy-then-delete

Tests and vignettes are *copied* (not moved) from `mighty` to `mightyverse` in the first phase. They are deleted from `mighty` only after `mightyverse`'s CI is green and the cross-repo CI from §2 is wired up and gating. This produces a temporary window of double-maintenance, which is acceptable in exchange for never having a window where integration tests are not enforced as a gate somewhere.

Cross-repo CI gating (§2) is a precondition for cleanup in `mighty`.

### 10. Versioning: floor pinning, tidyverse-style

`mightyverse` declares its sibling dependencies with `>=` constraints, not exact pins. `mightyverse` releases bump floors to whatever versions the siblings shipped most recently. This matches `tidyverse`'s own DESCRIPTION pattern.

The safety of floor pinning depends on §2 being in place. If §2 is not adopted, this recommendation should be revisited (likely toward exact pinning or synchronized releases).

### 11. Test fixtures: split by usage

Trace each fixture in `mighty/tests/testthat/fixtures/` and test helper function to its consumers. Fixtures used only by integration tests move to `mightyverse`. Fixtures used only by unit tests stay in `mighty`. Fixtures used by both are duplicated.

*Alternative considered:* extract shared fixtures into a separate `mighty.testdata` package. Rejected as over-engineering for the current size of the ecosystem; can be revisited if shared fixtures grow large.


### 12. Snapshots: move verbatim

`testthat` snapshot files (`_snaps/`) move along with their tests, unmodified. They are not regenerated as part of the migration. A snapshot mismatch after the move can be diagnostic if the move accidentally changed behavior.


## Non-goals

This ADR explicitly does not decide:

- Whether `mighty.standards` becomes a real R package with exports.
- Whether `mighty.toolbox` is opened up.
- Any change to `mighty`'s exported API (it remains at 2 exports).
- The plan for backfilling unit tests in `mighty` after the migration.
- Concrete CI workflow YAML; §2 sets the principle, the implementation is a follow-up.
- Branch protection and release-process adjustments. These should be picked up by the team after this ADR lands.

## Consequences

- `mighty`'s pkgdown site becomes a thin function reference (2 exports + dev docs) for some period after migration. This is honest about what `mighty` is, but is a visible change for anyone who currently uses `mighty`'s pkgdown as their entry point.
- `mightyverse` becomes a load-bearing piece of CI infrastructure for the whole ecosystem (§2). Outages in `mightyverse`'s `main` block sibling-repo PRs.
- A temporary window of double-maintenance during migration (§9) is the explicit cost of avoiding any ungated period.
