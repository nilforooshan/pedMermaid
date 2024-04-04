# pedMermaid

<!-- badges: start -->

[![R-CMD-check](https://github.com/nilforooshan/pedMermaid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nilforooshan/pedMermaid/actions/workflows/R-CMD-check.yaml)
[![cran-version-last-release](https://www.r-pkg.org/badges/version-last-release/pedMermaid)](https://cran.r-project.org/package=pedMermaid)
[![github-release](https://img.shields.io/github/release/nilforooshan/pedMermaid.svg)](https://github.com/nilforooshan/pedMermaid)
[![github-license](https://img.shields.io/github/license/nilforooshan/pedMermaid.svg)](https://github.com/nilforooshan/pedMermaid/blob/master/LICENSE)
[![github-contributors](https://img.shields.io/github/contributors/nilforooshan/pedMermaid.svg)](https://github.com/nilforooshan/pedMermaid/graphs/contributors/)

<!-- badges: end -->

R package pedMermaid generates [Mermaid](https://mermaid.js.org/) syntax for creating a pedigree flowchart in [Markdown](https://www.markdownguide.org/) and [R Markdown](https://rmarkdown.rstudio.com/), from a pedigree data frame.

## Installation

You can install the development version of pedMermaid like so:

```r
devtools::install_github('nilforooshan/ggroups')
```

## Example

This is a basic example of a pedigree flowchart created by the Mermaid syntax generated by the R package pedMermaid.

```r
library(pedMermaid)
ped <- data.frame(ID = 1:7,
                  SIRE = c(0, 0, 1, 0, 3, 0, 5),
                  DAM = c(0, 0, 2, 2, 4, 0, 6))
mermaid_md(ped)
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
However, the syntax is more restricted (less link customizations) and less compact.
The `mermaid_md` function provides the following customizations:

- Links
  - Orientation (top-to-bottom or left-to-right)
  - Type (arrow or line)
  - Curve
  - Dash (dashed or solid)
  - Line width
  - Color
- Nodes (node-specific)
  - Text color
  - Background color
  - Border color
  - Round border
  - Dashed (or solid) border
  - Line width of the border
