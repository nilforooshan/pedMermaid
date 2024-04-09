#' @title Generate Mermaid syntax for a pedigree flowchart in Markdown
#'
#' @param ped : A data frame with the mandatory columns ID, SIRE, and DAM,
#' and the optional columns TextColor, BgColor, BorderColor, RoundBorder, DashBorder, and lwd.
#' The optional columns define (child) node-specific shape and style (corresponding to the ID column).
#' The order of columns does not matter, but column names do matter and case-sensitive. \cr
#'
#' `ped` columns: \cr
#'
#' ID : Numeric or alphanumeric individual identifications. \cr
#'
#' SIRE : Sire identifications. Missing sires are denoted as 0. \cr
#'
#' DAM : Dam identifications. Missing dams are denoted as 0. \cr
#'
#' TextColor : Child (corresponding to ID) node's text color (valid color names and valid hex color codes).
#' If this column is missing, the default color (`"black"` or `"#000000"`) is used.
#' Also, `NA` enforces the default color. \cr
#'
#' BgColor : Child (corresponding to ID) node's background color (valid color names and valid hex color codes).
#' If this column is missing, the default color (`"#ECECFF"`) is used.
#' Also, `NA` enforces the default color. \cr
#'
#' BorderColor : Child (corresponding to ID) node's border color (valid color names and valid hex color codes).
#' If this column is missing, the default color (`"#9370DB"`) is used.
#' Also, `NA` enforces the default color. \cr
#'
#' RoundBorder : Child (corresponding to ID) node's border shape (90\eqn{\circ} vs rounded edges).
#' This column receives `"Y"` (rounded edges), `"N"` (90\eqn{\circ} edges), and `NA` (equivalent to the default value).
#' If this column is missing, the default border shape (`"N"`) is used. \cr
#'
#' DashBorder : Child (corresponding to ID) node's border line style (dashed vs solid).
#' This column receives `"Y"` (solid line), `"N"` (dashed line), and `NA` (equivalent to the default value).
#' If this column is missing, the default border line style (`"N"`) is used. \cr
#'
#' lwd : Child (corresponding to ID) node's border line width.
#' This column receives values > 0, \eqn{\leq} 5, and `NA` (equivalent to the default value).
#' If this column is missing, the default value (1) is used. \cr
#'
#' @param orient : Defines the orientation of the flowchart (`"TB"` for top-to-bottom vs `"LR"` for left-to-right).
#' If no input is provided, the default orientation (`"TB"`) is used.
#' @param type : Defines the type of links in the flowchart (`"arrow"` vs `"line"`).
#' If no input is provided, the default link type (`"arrow"`) is used.
#' @param curve : Defines the shape of links in the flowchart
#' (`"basis"`, `"bumpX"`, `"bumpY"`, `"cardinal"`, `"catmullRom"`, `"linear"`, `"monotoneX"`, `"monotoneY"`, `"natural"`, `"step"`, `"stepAfter"`, and `"stepBefore"`).
#' If no input is provided, the default link shape (`"basis"`) is used.
#' @param dash : Defines the style of links in the flowchart (`"N"` for solid line vs `"Y"` for dashed line).
#' If no input is provided, the default link style (`"N"`) is used.
#' @param lwd : Defines the width of links in the flowchart (> 0 and \eqn{\leq} 5).
#' If no input is provided, the default value (2) is used.
#' @param color : Defines the color of links in the flowchart (a valid color name or a valid hex color code).
#' If no input is provided, the default color (`"black"` or `"#000000"`) is used.
#'
#' @return : A vector of character strings. Each character string is a Mermaid syntax line.
#' Assuming the returned output is saved in object `x`,
#' use `cat(x, sep = "\n")` to display the output on-screen,
#' and `cat(x, sep = "\n", file = "output.txt")` or `write(x, file = "output.txt")` to write the output into a file.
#'
#' @examples
#' # A sample pedigree data frame with only the three mandatory columns.
#' ped <- data.frame(ID = 1:7, SIRE = c(0, 0, 1, 0, 3, 0, 5), DAM = c(0, 0, 2, 2, 4, 0, 6))
#'
#' # Example 1: A pedigree Mermaid syntax without customizations.
#' x <- mermaid_md(ped)
#' # Read the "Value" part about displaying the output on-screen and writing it into a file.
#'
#' # Example 2: Repeat example 1. Change arrow links to lines and the orientation to horizontal.
#' x <- mermaid_md(ped, orient = "LR", type = "line")
#'
#' # Example 3: Repeat example 1. Pink background and round border edges for females (2, 4, 6).
#' ped$BgColor <- c(NA, "pink", NA, "pink", NA, "pink", NA)
#' ped$RoundBorder <- c(NA, "Y", NA, "Y", NA, "Y", NA)
#' x <- mermaid_md(ped)
#'
#' # Example 4: Repeat example 3. Ticker border line for individuals in the control group (2, 5, 7).
#' ped$lwd <- c(1, 3, 1, 1, 3, 1, 3)
#' x <- mermaid_md(ped)
#'
#' # Example 5: Use the default value and NA alternatively. This is not different from example 4.
#' ped$lwd <- c(NA, 3, NA, NA, 3, NA, 3)
#' x <- mermaid_md(ped)
#'
#' # Example 6: Repeat example 1. Change link curve to "step" and green dashed.
#' x <- mermaid_md(ped[, c("ID", "SIRE", "DAM")],
#'     curve = "step", color = "#00FF00", dash = "Y")
#'
#' @export
mermaid_md <- function(ped, orient = "TB", type = "arrow", curve = "basis", dash = "N", lwd = 2, color = "black") {
    # Check inputs
    ## ped
    if (!is.data.frame(ped)) stop("The pedigree object is not a data.frame.")
    if (!all(c("ID", "SIRE", "DAM") %in% colnames(ped))) stop("ID, SIRE, and DAM are the mandatory columns of the pedigree data.frame, and should be present.")
    if (any(is.na(ped[, c("ID", "SIRE", "DAM")]))) stop("Found missing value(s) in the mandatory columns (ID, SIRE, and DAM) of the pedigree data.frame.")
    if (any(ped$ID == 0)) stop("Found 0 in the ID column of the pedigree data.frame.")
    if (any(duplicated(ped$ID))) stop("Found duplicates in the ID column of the pedigree data.frame.")
    if ("TextColor" %in% colnames(ped) & any(!is.color_name(unique(na.omit(ped$TextColor))) & !sapply(unique(na.omit(ped$TextColor)), is.hex_color))) {
        stop("Found an invalid color name/code in the TextColor column of the pedigree data.frame.")
    }
    if ("BgColor" %in% colnames(ped) & any(!is.color_name(unique(na.omit(ped$BgColor))) & !sapply(unique(na.omit(ped$BgColor)), is.hex_color))) {
        stop("Found an invalid color name/code in the BgColor column of the pedigree data.frame.")
    }
    if ("BorderColor" %in% colnames(ped) & any(!is.color_name(unique(na.omit(ped$BorderColor))) & !sapply(unique(na.omit(ped$BorderColor)), is.hex_color))) {
        stop("Found an invalid color name/code in the BorderColor column of the pedigree data.frame.")
    }
    if ("RoundBorder" %in% colnames(ped) & any(!unique(ped$RoundBorder) %in% c(NA, "N", "Y"))) {
        stop('Found an invalid element in the RoundBorder column of the pedigree data.frame. The acceptable elements are "Y", "N", and NA.')
    }
    if ("DashBorder" %in% colnames(ped) & any(!unique(ped$DashBorder) %in% c(NA, "Y", "N"))) {
        stop('Found an invalid element in the DashBorder column of the pedigree data.frame. The acceptable elements are "Y", "N", and NA.')
    }
    if ("lwd" %in% colnames(ped)) {
        if (min(na.omit(ped$lwd)) <= 0) stop("Found a value <= 0 in the lwd column of the pedigree data.frame.")
        if (max(na.omit(ped$lwd)) > 5) stop("Found a value > 5 in the lwd column of the pedigree data.frame.")
    }
    ## Other arguments
    for (i in c("orient", "type", "curve", "dash", "lwd", "color"))
    {
        if (length(get(i)) != 1) stop(paste0("length(", i, ") != 1"))
    }
    if (!orient %in% c("TB", "LR")) stop('The acceptable inputs to the orient argument are "TB" and "LR".')
    if (!type %in% c("arrow", "line")) stop('The acceptable inputs to the type argument are "arrow" and "line".')
    if (!curve %in% c("basis", "bumpX", "bumpY", "cardinal", "catmullRom", "linear", "monotoneX", "monotoneY", "natural", "step", "stepAfter", "stepBefore")) {
        stop('The acceptable inputs to the curve argument are "basis", "bumpX", "bumpY", "cardinal", "catmullRom", "linear", "monotoneX", "monotoneY", "natural", "step", "stepAfter", and "stepBefore".')
    }
    if (!dash %in% c("Y", "N")) stop('The acceptable inputs to the dash argument are "Y" and "N".')
    if (lwd <= 0 | lwd > 5) stop("The value provided to the lwd argument is not > 0 and <= 5.")
    if (!is.color_name(color) & !is.hex_color(color)) stop("The input provided to the color argument is not a valid color name or a valid hex color code.")

    # Modify inputs
    if (type == "arrow") {
        type <- " --> "
    } else {
        type <- " --- "
    }
    ## Set defaults to NA (nothing to paste using paste.na.omit())
    if (curve == "basis") curve <- NA
    if (dash == "N") dash <- NA
    if (lwd == 2) lwd <- NA
    if (color %in% c("black", "#000000")) color <- NA
    ### The mandatory columns of ped
    ped[ped$SIRE == 0, ]$SIRE <- NA
    ped[ped$DAM == 0, ]$DAM <- NA
    ### The optional columns of ped
    if ("TextColor" %in% colnames(ped)) {
        if (any(ped$TextColor %in% c("black", "#000000"))) ped[ped$TextColor %in% c("black", "#000000"), ]$TextColor <- NA
        if (all(is.na(ped$TextColor))) {
            ped$TextColor <- NULL # Drop the column
        } else {
            ped[!is.na(ped$TextColor), ]$TextColor <- paste0("color:", ped[!is.na(ped$TextColor), ]$TextColor)
        }
    }
    if ("BgColor" %in% colnames(ped)) {
        if (any(toupper(ped$BgColor) %in% "#ECECFF")) ped[toupper(ped$BgColor) %in% "#ECECFF", ]$BgColor <- NA
        if (all(is.na(ped$BgColor))) {
            ped$BgColor <- NULL # Drop the column
        } else {
            ped[!is.na(ped$BgColor), ]$BgColor <- paste0("fill:", ped[!is.na(ped$BgColor), ]$BgColor)
        }
    }
    if ("BorderColor" %in% colnames(ped)) {
        if (any(toupper(ped$BorderColor) %in% "#9370DB")) ped[toupper(ped$BorderColor) %in% "#9370DB", ]$BorderColor <- NA
        if (all(is.na(ped$BorderColor))) {
            ped$BorderColor <- NULL # Drop the column
        } else {
            ped[!is.na(ped$BorderColor), ]$BorderColor <- paste0("stroke:", ped[!is.na(ped$BorderColor), ]$BorderColor)
        }
    }
    if ("RoundBorder" %in% colnames(ped)) {
        if (any(ped$RoundBorder %in% "N")) ped[ped$RoundBorder %in% "N", ]$RoundBorder <- NA
        if (all(is.na(ped$RoundBorder))) ped$RoundBorder <- NULL # Drop the column
    }
    if ("DashBorder" %in% colnames(ped)) {
        if (any(ped$DashBorder %in% "N")) ped[ped$DashBorder %in% "N", ]$DashBorder <- NA
        if (all(is.na(ped$DashBorder))) {
            ped$DashBorder <- NULL # Drop the column
        } else {
            ped[ped$DashBorder %in% "Y", ]$DashBorder <- "stroke-dasharray:4"
        }
    }
    if ("lwd" %in% colnames(ped)) {
        if (any(ped$lwd %in% 1)) ped[ped$lwd %in% 1, ]$lwd <- NA
        if (all(is.na(ped$lwd))) {
            ped$lwd <- NULL # Drop the column
        } else {
            ped[!is.na(ped$lwd), ]$lwd <- paste0("stroke-width:", ped[!is.na(ped$lwd), ]$lwd)
        }
    }

    # Write init.line, if color and curve are different from defaults
    init.line <- NA
    color.code <- NA
    curve.code <- NA
    if (!is.na(color) | !is.na(curve)) {
        if (!is.na(color)) color.code <- paste0("'themeVariables': { 'lineColor': '", color, "' }")
        if (!is.na(curve)) curve.code <- paste0("'flowchart': { 'curve': '", curve, "' }")
        init.code <- paste.na.omit(c(color.code, curve.code), sep = ", ")
        init.line <- paste("%%{ init: {", init.code, "} }%%")
    }

    # Write flowchart.line
    flowchart.line <- paste("flowchart", orient)

    # Write linkStyle.line, if dash and lwd are different from defaults
    linkStyle.line <- NA
    stroke.dasharray <- NA
    stroke.width <- NA
    if (!is.na(dash) | !is.na(lwd)) {
        if (!is.na(dash)) stroke.dasharray <- "stroke-dasharray:3"
        if (!is.na(lwd)) stroke.width <- paste0("stroke-width:", lwd)
        linkStyle.code <- paste.na.omit(c(stroke.dasharray, stroke.width), sep = ",")
        linkStyle.line <- paste("linkStyle default", linkStyle.code)
        # Indent
        linkStyle.line <- paste("   ", linkStyle.line)
    }

    # Write classDef.lines, if any of the 5 of 6 optional columns exists
    classDef.lines <- NA
    if (any(c("TextColor", "BgColor", "BorderColor", "DashBorder", "lwd") %in% colnames(ped))) {
        for (i in c("TextColor", "BgColor", "BorderColor", "DashBorder", "lwd"))
        {
            # Create an NA column for the missing ones.
            if (!i %in% colnames(ped)) ped[, i] <- NA
        }
        ped$classDef <- apply(ped[, c("TextColor", "BgColor", "BorderColor", "DashBorder", "lwd")], 1, paste.na.omit, sep = ",")
        if (any(ped$classDef == "")) ped[ped$classDef == "", ]$classDef <- NA
        ped$classDef <- as.factor(ped$classDef)
        ped$classNum <- as.integer(ped$classDef)
        classDef.lines <- unique(paste0("    classDef class", ped[!is.na(ped$classNum), ]$classNum, " ", ped[!is.na(ped$classDef), ]$classDef))
    }

    # Write node.lines
    ## Filter ped rows depending on the presence of ped$RoundBorder and ped$classNum
    if ("RoundBorder" %in% colnames(ped) & "classNum" %in% colnames(ped)) {
        ped <- ped[!is.na(ped$SIRE) | !is.na(ped$DAM) | !is.na(ped$RoundBorder) | !is.na(ped$classNum), ]
    } else if ("RoundBorder" %in% colnames(ped) & !"classNum" %in% colnames(ped)) {
        ped <- ped[!is.na(ped$SIRE) | !is.na(ped$DAM) | !is.na(ped$RoundBorder), ]
    } else if (!"RoundBorder" %in% colnames(ped) & "classNum" %in% colnames(ped)) {
        ped <- ped[!is.na(ped$SIRE) | !is.na(ped$DAM) | !is.na(ped$classNum), ]
    } else {
        ped <- ped[!is.na(ped$SIRE) | !is.na(ped$DAM), ]
    }
    ## Create parent and child nodes
    node.lines <- ped[, colnames(ped) %in% c("ID", "SIRE", "DAM", "RoundBorder", "classNum")]
    colnames(node.lines)[colnames(node.lines) == "ID"] <- "childnode"
    node.lines$parentnodes <- apply(node.lines[, c("SIRE", "DAM")], 1, paste.na.omit, sep = " & ")
    if (any(node.lines$parentnodes == "")) node.lines[node.lines$parentnodes == "", ]$parentnodes <- NA
    node.lines <- node.lines[, !colnames(node.lines) %in% c("SIRE", "DAM")]
    ## If RoundBorder column exists, apply it
    if ("RoundBorder" %in% colnames(node.lines) & any(node.lines$RoundBorder %in% "Y")) {
        node.lines[node.lines$RoundBorder %in% "Y", ]$childnode <- paste0(
            node.lines[node.lines$RoundBorder %in% "Y", ]$childnode, "(",
            node.lines[node.lines$RoundBorder %in% "Y", ]$childnode, ")"
        )
    }
    ## If classNum column exists, apply it
    if ("classNum" %in% colnames(node.lines)) {
        node.lines[!is.na(node.lines$classNum), ]$childnode <- paste0(
            node.lines[!is.na(node.lines$classNum), ]$childnode, ":::class",
            node.lines[!is.na(node.lines$classNum), ]$classNum
        )
    }
    ## Link parentnodes to childnode
    node.lines[!is.na(node.lines$parentnodes), ]$childnode <- paste(node.lines[!is.na(node.lines$parentnodes), ]$parentnodes,
        node.lines[!is.na(node.lines$parentnodes), ]$childnode,
        sep = type
    )
    node.lines <- node.lines$childnode
    ## Indent
    node.lines <- paste("   ", node.lines)

    # Cocatenate lines into the output
    output <- c("```mermaid", na.omit(c(init.line, flowchart.line, linkStyle.line, node.lines, classDef.lines)), "```")

    # Return the output
    return(output)
}
