#' @export
substances.path <- function() system.file("extdata", "substances.xlsx", package = "PesticideLoadIndicator", mustWork=TRUE);

#' @export
substances.load <- function() read.excel(substances.path());

#' @export
products.path <- function() system.file("extdata", "products.xlsx", package = "PesticideLoadIndicator", mustWork=TRUE);

#' @export
products.load <- function() read.excel(products.path());