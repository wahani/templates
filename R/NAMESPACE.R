#' @importFrom magrittr %>%
#' @importFrom stats update as.formula
NULL

# resolves dependency to dat
flatmap <- function(...) unlist(lapply(...))
extract <- function(x, ...) x[...]
extract2 <- function(x, ...) extract(x, ...)[[1]]
