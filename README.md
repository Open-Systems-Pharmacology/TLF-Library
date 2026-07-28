# tlf: Table, Listing and Figure for OSP

<!-- badges: start -->

[![Build](https://github.com/Open-Systems-Pharmacology/TLF-Library/actions/workflows/main-workflow.yaml/badge.svg)](https://github.com/Open-Systems-Pharmacology/TLF-Library/actions/workflows/main-workflow.yaml)
<a
href="https://app.codecov.io/gh/Open-Systems-Pharmacology/TLF-Library"
class="pkgdown-devel"><img
src="https://codecov.io/gh/Open-Systems-Pharmacology/TLF-Library/branch/develop/graph/badge.svg"
alt="codecov" /></a>

<!-- badges: end -->

The `{tlf}` package provides an object-oriented framework to create
tables and figures, which are used by R packages in the Open Systems
Pharmacology ecosystem:

-   [`{ospsuite}`](https://www.open-systems-pharmacology.org/OSPSuite-R/)
-   [`{ospsuite.reportingengine}`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/)

## Installation

`{tlf}` and its Open Systems Pharmacology dependencies are published on the
[OSP R-universe](https://open-systems-pharmacology.r-universe.dev). Installing
the released version needs nothing but base R, and resolves
`{ospsuite.utils}` for you:

``` r
install.packages(
  "tlf",
  repos = c(OSP = "https://open-systems-pharmacology.r-universe.dev", getOption("repos"))
)
```

To install the development version from GitHub instead, use
[pak](https://pak.r-lib.org):

``` r
# install.packages("pak")
pak::pak("Open-Systems-Pharmacology/TLF-Library")
```

Binaries also remain attached to every
[GitHub release](https://github.com/Open-Systems-Pharmacology/TLF-Library/releases),
and can be installed from a local file with
`install.packages(pathToZip, repos = NULL)`.

`{tlf}` requires the following packages, which the commands above install for
you:

From CRAN:

-   [ggplot2](https://cran.r-project.org/package=ggplot2/index.html)
-   [jsonlite](https://cran.r-project.org/package=jsonlite/index.html)
-   [patchwork](https://cran.r-project.org/package=patchwork/index.html)
-   [R6](https://cran.r-project.org/package=R6/index.html)
-   [ggtext](https://cran.r-project.org/package=ggtext/index.html)
-   [stringr](https://cran.r-project.org/package=stringr/index.html)
-   [rlang](https://cran.r-project.org/package=rlang/index.html)
-   [lifecycle](https://cran.r-project.org/package=lifecycle/index.html)

From the OSP R-universe:

-   [ospsuite.utils](https://open-systems-pharmacology.r-universe.dev/ospsuite.utils)

## Documentation

A detailed account of existing functions and articles on how to use them
can be found on the [dedicated
website](https://www.open-systems-pharmacology.org/TLF-Library/).

## Code of conduct

Everyone interacting in the Open Systems Pharmacology community
(codebases, issue trackers, chat rooms, mailing lists etc.) is expected
to follow the Open Systems Pharmacology [code of
conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

## Contribution

We encourage contribution to the Open Systems Pharmacology community.
Before getting started please read the [contribution
guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md).
If you are contributing code, please be familiar with the [coding
standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS_R.md).

## License

TLF Library is released under the [GPLv2 License](LICENSE).

All trademarks within this document belong to their legitimate owners.

