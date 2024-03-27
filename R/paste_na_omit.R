#' @title Paste vector elements omit NAs
#'
#' @noRd
paste.na.omit <- function(vec, sep=" ") return(paste(vec[!is.na(vec)], collapse=sep))
