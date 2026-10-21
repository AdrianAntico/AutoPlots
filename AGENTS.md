# AutoPlots Agent Guide

## Mission
Reusable high-level visualization, themes, formatting defaults, htmlwidgets, sizing, and display composition for the suite.

## Owns
- Plot functions, ECharts option exposure, themes, axis/tooltip defaults, widget behavior, and `display_*` helpers.

## Plot-function authority
- `R/PlotFunctions_NEW.R` is the canonical modern plot-function authority (`Line()`, `Bar()`, `Density()`, `Scatter()`, `Histogram()`, and the rest of the short-name API).
- `R/PlotFunctions.R` is legacy/stale (`Plot.Line()`, `Plot.Bar()`, ...). Treat it as archaeology unless a task explicitly targets legacy compatibility.
- New capabilities must reuse or extend the modern AutoPlots API. Do not copy legacy `Plot.*` primitives, do not add a competing chart layer, and do not use `PlotFunctions.R` as the integration reference.
- Public AutoPlots must not absorb private analytical implementations, specialized restricted views, or reconstruction-grade fixtures.

## Must Not Own
- Analytical algorithms, report-plan semantics, Workstation state, or domain conclusions.
- Product/domain repos must not duplicate reusable plots that belong here.

## Canonical Suite Context
Read `../AnalyticsWorkstation-Development/app/architecture/docs/analytics_workstation_engineering_constitution_1_0_2026-08-15.md` and Suite State before changing cross-package display contracts.

## Local Validation
```r
devtools::load_all()
qa_autoplots_package()
devtools::test()
```
Use `devtools::document()` after roxygen changes and `R CMD check` for release validation.

## Runtime and Dependencies
- R/htmlwidgets/ECharts implementation. Package theme helpers remain authoritative; callers supply data and intent.
- Preserve Shiny and static-report sizing, resize, and accessibility behavior.

## Development Invariants
- Inspect theme/default application before forcing labels, rotations, tooltips, or dimensions downstream.
- Repair reusable display behavior here, add focused visual/structural regression, and preserve existing high-level APIs.
- Do not claim visual closure without the relevant browser/static render evidence.

## Git and Safety
- Preserve unrelated dirty work and generated browser output. No revert, stage, commit, push, or force-push unless instructed.
- No secrets or machine paths. Stop and ask before moving analytical meaning into visualization code.

## Primary checkouts and agent workspaces

Keep the user's designated primary checkout on the canonical published default
branch (`main`). Do agent development and qualification in isolated worktrees or
clones outside the primary checkout, using a `codex/` or other explicitly assigned
development branch. Do not leave the primary
checkout on a temporary development branch.

Before alignment, record the branch, HEAD, remote, dirty/staged/untracked state,
potentially affected ignored files, linked worktrees, and active users/processes.
Preserve changes and development history in verified recoverable archives or refs.
Never discard work or merge unqualified development merely to align the checkout.
Fetch the actual remote default branch and use a non-force fast-forward only.
Keep exactly one designated primary AutoPlots checkout at its original user path.
If an active session prevents safe alignment, stop consolidation until that session
releases the checkout. Do not create a second permanent primary directory. Retire
any temporary duplicate only after the original path is verified, preserving unique
work and linked worktrees through a reversible archive.

At completion verify the designated primary path, branch, local and remote HEAD,
working-tree status, preservation locations, and the source used for any local
installation. Do not switch or terminate another agent's active environment.
