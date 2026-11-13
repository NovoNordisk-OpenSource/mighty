## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for reviewers

* The example for `generate_adam_code()` is wrapped in `@examplesIf interactive()`, and tests that need components
  from GitHub use `skip_on_cran()`. These components come from a GitHub repository
  (NovoNordisk-OpenSource/mighty.standards) that is not yet on CRAN, so they need network access.
