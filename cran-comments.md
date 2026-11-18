## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for reviewers

* The example for `generate_adam_code()` and a smoke test use a small example study in `inst/examples/offline`,
  which needs no network access. Further tests use components from a GitHub repository
  (NovoNordisk-OpenSource/mighty.standards) that is not on CRAN, so they need network access and use `skip_on_cran()`.
