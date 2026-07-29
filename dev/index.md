# tlf: Table, Listing and Figure for OSP

The [tlf](https://github.com/open-systems-pharmacology/tlf-library)
package provides an object-oriented framework to create tables and
figures, which are used by R packages in the Open Systems Pharmacology
ecosystem:

- [`{ospsuite}`](https://www.open-systems-pharmacology.org/OSPSuite-R/)
- [`{ospsuite.reportingengine}`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/)

## Installation

[tlf](https://github.com/open-systems-pharmacology/tlf-library) and its
Open Systems Pharmacology dependencies are published on the [OSP
R-universe](https://open-systems-pharmacology.r-universe.dev).
Installing the released version needs nothing but base R, and resolves
[ospsuite.utils](https://github.com/open-systems-pharmacology/OSPSuite.RUtils)
for you:

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

TLF Library is released under the [GPLv2
License](https://www.open-systems-pharmacology.org/TLF-Library/dev/LICENSE).

All trademarks within this document belong to their legitimate owners.
