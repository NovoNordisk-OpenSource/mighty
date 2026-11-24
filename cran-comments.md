## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for reviewers

* mighty generates R programs by combining code components. The framework components it always needs are maintained
  in a GitHub repository (NovoNordisk-OpenSource/mighty.standards) that is not on CRAN, so generating a program
  requires network access.
* Without network access `generate_adam_code()` stops with an informative error.
* For that reason the example for `generate_adam_code()` is wrapped in `@examplesIf interactive()`. The example for
  `write_adam_programs()` runs offline.
* Tests that need the components from GitHub use `skip_on_cran()`. The remaining 324 tests run offline (100 are
  skipped on CRAN).
