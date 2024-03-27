#' @title Check if a hex color code is valid
#'
#' @noRd
is.hex_color <- function(color_code) {
    # Regular expression to match HEX color code pattern
    regex <- "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$"

    # Check if the color code matches the pattern
    if (grepl(regex, color_code)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
