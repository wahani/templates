#' Templates constructors
#'
#' @param x
#'
#' @export
#' @rdname templates
templateChar <- function(x) {
  addClass(x, c("templateChar", "template"))
}

#' @export
#' @rdname templates
templateExpr <- function(x) {

  deparse(substitute(x)) %>%
    paste(collapse = "\n") %>%
    stringr::str_replace_all("\\{\\\n\ +\\{", "{{") %>%
    stringr::str_replace_all("\\}\\\n\ +\\}", "}}") %>%
    templateChar %>%
    addClass("templateExpr")
  
}

#' @export
#' @rdname templates
templateFun <- function(x) {
  dat:::addClass(x, "templateFun")
}

#' @export
as.function.templateChar <- function(x, ..., parent = parent.frame()) {
  
  if (length(list(...)) > 0) {
    x <- templateSub(x, ...)
  }    
  
  templateAsFun(x, parent)
  
}

#' @export
as.function.templateFun <- function(x, ..., parent = parent.frame()) {
  templateSub(x, ...)
}

#' @export
print.template <- function(x, ...) {
  cat("\n", x, "\n")
  x
}
