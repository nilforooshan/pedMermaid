# pedMermaid

<!-- badges: start -->

[![R-CMD-check](https://github.com/nilforooshan/pedMermaid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nilforooshan/pedMermaid/actions/workflows/R-CMD-check.yaml)
[![cran-version-last-release](https://www.r-pkg.org/badges/version-last-release/pedMermaid)](https://cran.r-project.org/package=pedMermaid)
[![cran-downloads-total](https://cranlogs.r-pkg.org/badges/grand-total/pedMermaid?color=green)](https://cran.r-project.org/package=pedMermaid)
[![cran-downloads-month](https://cranlogs.r-pkg.org/badges/last-month/pedMermaid?color=green)](https://cran.r-project.org/package=pedMermaid)
[![cran-downloads-week](https://cranlogs.r-pkg.org/badges/last-week/pedMermaid?color=green)](https://cran.r-project.org/package=pedMermaid)
[![github-release](https://img.shields.io/github/release/nilforooshan/pedMermaid.svg)](https://github.com/nilforooshan/pedMermaid)
[![github-license](https://img.shields.io/github/license/nilforooshan/pedMermaid.svg)](https://github.com/nilforooshan/pedMermaid/blob/master/LICENSE.md)
[![github-contributors](https://img.shields.io/github/contributors/nilforooshan/pedMermaid.svg)](https://github.com/nilforooshan/pedMermaid/graphs/contributors/)

<!-- badges: end -->

R package pedMermaid generates [Mermaid](https://mermaid.js.org/) syntax for creating a pedigree flowchart in [Markdown](https://www.markdownguide.org/) and [R Markdown](https://rmarkdown.rstudio.com/), from a pedigree data frame.

## Installation

Install the latest official version from CRAN:

```r
install.packages("pedMermaid")
```

Install the latest development version (if different from the official version) from GitHub:

```r
devtools::install_github('nilforooshan/pedMermaid')
```

## Example

This is a basic example of a pedigree flowchart created by the Mermaid syntax generated by the R package pedMermaid.

```r
library(pedMermaid)
ped <- data.frame(ID = 1:7,
                  SIRE = c(0, 0, 1, 0, 3, 0, 5),
                  DAM = c(0, 0, 2, 2, 4, 0, 6))
x <- mermaid_md(ped)
# cat(x, sep = "\n") # Display the output syntax on-screen
```

```mermaid
flowchart TB
    1 & 2 --> 3
    2 --> 4
    3 & 4 --> 5
    5 & 6 --> 7
```

R package pedMermaid is equipped with functions `mermaid_md` and `mermaid_rmd` for generating Mermaid flowchart syntax for Markdown and R Markdown, respectively.
The syntax generated by `mermaid_rmd` is also applicable to Markdown.
However, the syntax is more restricted (less customizations) and less compact.
The `mermaid_md` function provides the following customizations:

- Links
  - Orientation (top-to-bottom or left-to-right)
  - Type (arrow or line)
  - Curve (not supported by `mermaid_rmd`)
  - Dash (dashed or solid) (not supported by `mermaid_rmd`)
  - Line width (not supported by `mermaid_rmd`)
  - Color (not supported by `mermaid_rmd`)
- Nodes (node-specific)
  - Text color (not supported by `mermaid_rmd`)
  - Background color
  - Border color
  - Round border
  - Dashed (or solid) border
  - Line width of the border
