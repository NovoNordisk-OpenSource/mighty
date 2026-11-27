# mighty (development version)

* Generated programs now start with a header listing the dataset they create, the datasets they read, the time generation was started (in UTC, the same for all programs from one run) and the mighty version. In `executable_programs`, datasets missing from the available data are marked as `(not available)` (#57).

* The program header names the programmer: the author of the last git commit that changed the domain's specification file, read with the gert package (no git installation needed). It shows `Not specified` when gert is not installed, the file is not committed or has uncommitted changes, or the repository is a shallow clone.

# mighty 0.1.0

* Initial CRAN submission.
