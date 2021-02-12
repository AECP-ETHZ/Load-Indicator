#' @export
ecotox.path <- function() system.file("extdata", "Ecotox.xlsx", package = "PesticideLoadIndicator", mustWork=TRUE);

#' @export
ecotox.load <- function() read.excel(ecotox.path());

