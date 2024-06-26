% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mermaid_md.R
\name{mermaid_md}
\alias{mermaid_md}
\title{Generate Mermaid syntax for a pedigree flowchart in Markdown}
\usage{
mermaid_md(
  ped,
  orient = "TB",
  type = "arrow",
  curve = "basis",
  dash = "N",
  lwd = 2,
  color = "black"
)
}
\arguments{
\item{ped}{: A data frame with the mandatory columns ID, SIRE, and DAM,
and the optional columns TextColor, BgColor, BorderColor, RoundBorder, DashBorder, and lwd.
The optional columns define (child) node-specific shape and style (corresponding to the ID column).
The order of columns does not matter, but column names do matter and case-sensitive. \cr

\code{ped} columns: \cr

ID : Numeric or alphanumeric individual identifications. \cr

SIRE : Sire identifications. Missing sires are denoted as 0. \cr

DAM : Dam identifications. Missing dams are denoted as 0. \cr

TextColor : Child (corresponding to ID) node's text color (valid color names and valid hex color codes).
If this column is missing, the default color (\code{"black"} or \code{"#000000"}) is used.
Also, \code{NA} enforces the default color. \cr

BgColor : Child (corresponding to ID) node's background color (valid color names and valid hex color codes).
If this column is missing, the default color (\code{"#ECECFF"}) is used.
Also, \code{NA} enforces the default color. \cr

BorderColor : Child (corresponding to ID) node's border color (valid color names and valid hex color codes).
If this column is missing, the default color (\code{"#9370DB"}) is used.
Also, \code{NA} enforces the default color. \cr

RoundBorder : Child (corresponding to ID) node's border shape (90\eqn{\circ} vs rounded edges).
This column receives \code{"Y"} (rounded edges), \code{"N"} (90\eqn{\circ} edges), and \code{NA} (equivalent to the default value).
If this column is missing, the default border shape (\code{"N"}) is used. \cr

DashBorder : Child (corresponding to ID) node's border line style (dashed vs solid).
This column receives \code{"Y"} (solid line), \code{"N"} (dashed line), and \code{NA} (equivalent to the default value).
If this column is missing, the default border line style (\code{"N"}) is used. \cr

lwd : Child (corresponding to ID) node's border line width.
This column receives values > 0, \eqn{\leq} 5, and \code{NA} (equivalent to the default value).
If this column is missing, the default value (1) is used. \cr}

\item{orient}{: Defines the orientation of the flowchart (\code{"TB"} for top-to-bottom vs \code{"LR"} for left-to-right).
If no input is provided, the default orientation (\code{"TB"}) is used.}

\item{type}{: Defines the type of links in the flowchart (\code{"arrow"} vs \code{"line"}).
If no input is provided, the default link type (\code{"arrow"}) is used.}

\item{curve}{: Defines the shape of links in the flowchart
(\code{"basis"}, \code{"bumpX"}, \code{"bumpY"}, \code{"cardinal"}, \code{"catmullRom"}, \code{"linear"}, \code{"monotoneX"}, \code{"monotoneY"}, \code{"natural"}, \code{"step"}, \code{"stepAfter"}, and \code{"stepBefore"}).
If no input is provided, the default link shape (\code{"basis"}) is used.}

\item{dash}{: Defines the style of links in the flowchart (\code{"N"} for solid line vs \code{"Y"} for dashed line).
If no input is provided, the default link style (\code{"N"}) is used.}

\item{lwd}{: Defines the width of links in the flowchart (> 0 and \eqn{\leq} 5).
If no input is provided, the default value (2) is used.}

\item{color}{: Defines the color of links in the flowchart (a valid color name or a valid hex color code).
If no input is provided, the default color (\code{"black"} or \code{"#000000"}) is used.}
}
\value{
: A vector of character strings. Each character string is a Mermaid syntax line.
Assuming the returned output is saved in object \code{x},
use \code{cat(x, sep = "\\n")} to display the output on-screen,
and \code{cat(x, sep = "\\n", file = "output.txt")} or \code{write(x, file = "output.txt")} to write the output into a file.
}
\description{
Generate Mermaid syntax for a pedigree flowchart in Markdown
}
\examples{
# A sample pedigree data frame with only the three mandatory columns.
ped <- data.frame(ID = 1:7, SIRE = c(0, 0, 1, 0, 3, 0, 5), DAM = c(0, 0, 2, 2, 4, 0, 6))

# Example 1: A pedigree Mermaid syntax without customizations.
x <- mermaid_md(ped)
# Read the "Value" part about displaying the output on-screen and writing it into a file.

# Example 2: Repeat example 1. Change arrow links to lines and the orientation to horizontal.
x <- mermaid_md(ped, orient = "LR", type = "line")

# Example 3: Repeat example 1. Pink background and round border edges for females (2, 4, 6).
ped$BgColor <- c(NA, "pink", NA, "pink", NA, "pink", NA)
ped$RoundBorder <- c(NA, "Y", NA, "Y", NA, "Y", NA)
x <- mermaid_md(ped)

# Example 4: Repeat example 3. Ticker border line for individuals in the control group (2, 5, 7).
ped$lwd <- c(1, 3, 1, 1, 3, 1, 3)
x <- mermaid_md(ped)

# Example 5: Use the default value and NA alternatively. This is not different from example 4.
ped$lwd <- c(NA, 3, NA, NA, 3, NA, 3)
x <- mermaid_md(ped)

# Example 6: Repeat example 1. Change link curve to "step" and green dashed.
x <- mermaid_md(ped[, c("ID", "SIRE", "DAM")],
    curve = "step", color = "#00FF00", dash = "Y")

}
