#' Title Produce Mermaid syntax for a pedigree flowchart
#'
#' @param ped : A data frame with the mandatory columns ID, SIRE, and DAM,
#' and the optional columns TextColor, BgColor, BorderColor, RoundBorder, DashBorder, and lwd.
#' The optional columns define (child) node-specific shape and style (corresponding to the ID column).
#' The order of columns does not matter, but column names do matter and case-sensitive. \cr
#'
#' \code{ped} columns: \cr
#'
#' ID : Numeric or alphanumeric individual identifications. \cr
#'
#' SIRE : Sire identifications. Missing sires are denoted as 0. \cr
#'
#' DAM : Dam identifications. Missing dams are denoted as 0. \cr
#'
#' TextColor : Child (corresponding to ID) node's text color (valid color names and valid hex color codes).
#' If this column is missing, the default color (\code{"black"} or \code{"#000000"}) is used.
#' Also, \code{NA} enforces the default color. \cr
#'
#' BgColor : Child (corresponding to ID) node's background color (valid color names and valid hex color codes).
#' If this column is missing, the default color (\code{"#ECECFF"}) is used.
#' Also, \code{NA} enforces the default color. \cr
#'
#' BorderColor : Child (corresponding to ID) node's border color (valid color names and valid hex color codes).
#' If this column is missing, the default color (\code{"#9370DB"}) is used.
#' Also, \code{NA} enforces the default color. \cr
#'
#' RoundBorder : Child (corresponding to ID) node's border shape (90\eqn{\circ} vs rounded edges).
#' This column receives \code{"Y"} (rounded edges), \code{"N"} (90\eqn{\circ} edges), and \code{NA} (equivalent to the default value).
#' If this column is missing, the default border shape (\code{"N"}) is used. \cr
#'
#' DashBorder : Child (corresponding to ID) node's border line style (dashed vs solid).
#' This column receives \code{"Y"} (solid line), \code{"N"} (dashed line), and \code{NA} (equivalent to the default value).
#' If this column is missing, the default border line style (\code{"N"}) is used. \cr
#'
#' lwd : Child (corresponding to ID) node's border line width.
#' This column receives values > 0, \eqn{\leq} 5, and \code{NA} (equivalent to the default value).
#' If this column is missing, the default value (1) is used. \cr
#'
#' @param orient : Defines the orientation of the flowchart (\code{"TB"} for top-to-bottom vs \code{"LR"} for left-to-right).
#' If no input is provided, the default orientation (\code{"TB"}) is used.
#' @param type : Defines the type of links in the flowchart (\code{"arrow"} vs \code{"line"}).
#' If no input is provided, the default link type (\code{"arrow"}) is used.
#' @param curve : Defines the shape of links in the flowchart
#' (\code{"basis"}, \code{"bumpX"}, \code{"bumpY"}, \code{"cardinal"}, \code{"catmullRom"}, \code{"linear"}, \code{"monotoneX"}, \code{"monotoneY"}, \code{"natural"}, \code{"step"}, \code{"stepAfter"}, and \code{"stepBefore"}).
#' If no input is provided, the default link shape (\code{"basis"}) is used.
#' @param dash : Defines the style of links in the flowchart (\code{"N"} for solid line vs \code{"Y"} for dashed line).
#' If no input is provided, the default link style (\code{"N"}) is used.
#' @param lwd : Defines the width of links in the flowchart (> 0 and \eqn{\leq} 5).
#' If no input is provided, the default value (2) is used.
#' @param color : Defines the color of links in the flowchart (a valid color name or a valid hex color code).
#' If no input is provided, the default color (\code{"black"} or \code{"#000000"}) is used.
#' @param outfile : The name of the output file, should the user want to pipe the output to an external file.
#' If no filename is provided, the output will be printed on the console.
#'
#' @return : Mermaid syntax for the pedigree flowchart.
#'
#' @examples
#' # A sample pedigree data frame with only the three mandatory columns
#' ped <- data.frame(ID=1:7, SIRE=c(0,0,1,0,3,0,5), DAM=c(0,0,2,2,4,0,6))
#' mermaid(ped)
#'
#' @examples
#' # TODO: More examples
#'
#' @export
mermaid <- function(ped, orient = "TB", type = "arrow", curve = "basis", dash = "N", lwd = 2, color = "black", outfile = NULL) {
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
    ## The mandatory columns of ped
    ped[ped$SIRE == 0, ]$SIRE <- NA
    ped[ped$DAM == 0, ]$DAM <- NA
    ## The optional columns of ped
    if ("TextColor" %in% colnames(ped)) {
        if (any(ped$TextColor %in% c("black", "#000000"))) ped[ped$TextColor %in% c("black", "#000000"), ]$TextColor <- NA
        ped[!is.na(ped$TextColor), ]$TextColor <- paste0("color:", ped[!is.na(ped$TextColor), ]$TextColor)
    }
    if ("BgColor" %in% colnames(ped)) {
        if (any(toupper(ped$BgColor) == "#ECECFF")) ped[toupper(ped$BgColor) %in% "#ECECFF", ]$BgColor <- NA
        ped[!is.na(ped$BgColor), ]$BgColor <- paste0("fill:", ped[!is.na(ped$BgColor), ]$BgColor)
    }
    if ("BorderColor" %in% colnames(ped)) {
        if (any(toupper(ped$BorderColor) == "#9370DB")) ped[toupper(ped$BorderColor) %in% "#9370DB", ]$BorderColor <- NA
        ped[!is.na(ped$BorderColor), ]$BorderColor <- paste0("stroke:", ped[!is.na(ped$BorderColor), ]$BorderColor)
    }
    if ("RoundBorder" %in% colnames(ped) & any(ped$RoundBorder == "N")) ped[ped$RoundBorder %in% "N", ]$RoundBorder <- NA
    if ("DashBorder" %in% colnames(ped)) {
        if (any(ped$DashBorder == "N")) ped[ped$DashBorder %in% "N", ]$DashBorder <- NA
        if (any(ped$DashBorder == "Y")) ped[ped$DashBorder %in% "Y", ]$DashBorder <- "stroke-dasharray:4"
    }
    if ("lwd" %in% colnames(ped)) {
        if (any(ped$lwd == 1)) ped[ped$lwd %in% 1, ]$lwd <- NA
        ped[!is.na(ped$lwd), ]$lwd <- paste0("stroke-width:", ped[!is.na(ped$lwd), ]$lwd)
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
        linkStyle.line <- paste("    ", linkStyle.line)
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
    ## Depending whether ped$RoundBorder or ped$classNum exist
    if (any(c("RoundBorder", "classNum") %in% colnames(ped))) {
        # Discard individuals that are not parent nor progeny of known parents
        ped <- ped[!is.na(ped$SIRE) | !is.na(ped$DAM) | ped$ID %in% unique(na.omit(ped$SIRE)) | ped$ID %in% unique(na.omit(ped$DAM)), ]
        # Create parent and child nodes
        node.lines <- data.frame(parentnodes = apply(ped[, c("SIRE", "DAM")], 1, paste.na.omit, sep = " & "), childnode = ped$ID)
        node.lines[node.lines$parentnodes == "", ]$parentnodes <- NA
        # If ped$RoundBorder exists, apply it
        if ("RoundBorder" %in% colnames(ped) & any(ped$RoundBorder == "Y")) {
            node.lines[ped$RoundBorder %in% "Y", ]$childnode <- paste0(node.lines[ped$RoundBorder %in% "Y", ]$childnode, "(", node.lines[ped$RoundBorder %in% "Y", ]$childnode, ")")
            ped[ped$RoundBorder %in% "Y", ]$RoundBorder <- paste0("(", ped[ped$RoundBorder %in% "Y", ]$ID, ")")
        }
        # If ped$classNum exists, apply it
        if ("classNum" %in% colnames(ped)) node.lines[!is.na(ped$classNum), ]$childnode <- paste0(node.lines[!is.na(ped$classNum), ]$childnode, ":::class", ped[!is.na(ped$classNum), ]$classNum)
        # Link parentnodes to childnode
        node.lines[!is.na(node.lines$parentnodes), ]$childnode <- paste(node.lines[!is.na(node.lines$parentnodes), ]$parentnodes, node.lines[!is.na(node.lines$parentnodes), ]$childnode, sep = type)
        node.lines <- node.lines$childnode
    } else {
        # Discard individuals without a known parent
        ped <- ped[!is.na(ped$SIRE) | !is.na(ped$DAM), ]
        # Create parent and child nodes
        node.lines <- data.frame(parentnodes = apply(ped[, c("SIRE", "DAM")], 1, paste.na.omit, sep = " & "), childnode = ped$ID)
        # Link parentnodes to childnode
        node.lines <- paste(node.lines$parentnodes, node.lines$childnode, sep = type)
    }
    ## Indent
    node.lines <- paste("    ", node.lines)

    # Cocatenate lines into the output
    output <- na.omit(c(init.line, flowchart.line, linkStyle.line, node.lines, classDef.lines))

    # Print the output
    if (is.null(outfile)) {
        cat(output, sep = "\n")
    } else {
        cat(output, sep = "\n", file = outfile)
    }
}
