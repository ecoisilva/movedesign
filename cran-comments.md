
# Version 0.3.1
## Re-submission

### R CMD check results

0 errors | 0 warnings | 1 note

There was one NOTE:

```
❯ checking for future file timestamps ... NOTE
  unable to verify current time
```

### Reviewer comments and responses

*Size of tarball: 10559599 bytes*
*A CRAN package should not be larger than 5 MB. Please reduce the size.*

- I have reduced the package size to below the 5 MB limit.

*Please do not start the description with "An R package for", package name, title or similar.*

- I have revised the description accordingly.

*Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)*

- I have added the missing \value tags to the `run_app.Rd` file.

*Please write references in the description of the DESCRIPTION file in the form*
*authors (year) <doi:...>*
*authors (year, ISBN:...)*
*or if those are not available: authors (year) <https:...>*

- All references are now properly formatted.

*Please add small executable examples in your Rd-files to illustrate the use of the exported function but also enable automatic testing.*
*Functions which are supposed to only run interactively (e.g. shiny) should be wrapped in if(interactive()). Please replace /dontrun{} with if(interactive()){} if possible, then users can see that the functions are not intended for use in scripts.*

- Examples have been updated. Interactive-only functions are now wrapped in `if(interactive())`.

*Please ensure that you do not use more than 2 cores in your examples, vignettes, etc.*

- All examples and vignettes do not use parallel processing and do not require multiple cores.

*Please fix and resubmit.*

### Downstream dependencies
There are no downstream dependencies.

---

# Version 0.3.0
## Initial submission

### R CMD check results

0 errors | 0 warnings | 1 note
* This is a new release.
