## Information
Re-submission of fixed package, which was archived on 2026-02-14 due to check
issues (not passing `R CMD check` with `_R_CHECK_DEPENDS_ONLY_=true`). Update
fixes problems in examples and tests by using `requireNamespace` for packages in
`Suggests` used in examples and tests. In addition, several small bugfixes are
included since the last release.

## Test environments
* local Fedora 43 (x86_64-redhat-linux-gnu), R 4.5.2 
* devtools::check_win_devel()

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 
