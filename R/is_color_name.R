#' @title Check if a color name is valid
#'
#' @noRd
is.color_name <- function(color_name) {
    valid_colors <- colors()
    return(color_name %in% valid_colors)
}
