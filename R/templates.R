#' Templates constructors
#'
#' @param x (character | expression)
#' @param envir (environment)
#'
#' @export
#' @rdname templates
template <- function(x, envir = parent.frame()) {

  constructor <- function(x, envir) {
    addAttr(x, class = "template", envir = envir)
  }

  x <- lazyeval::lazy(x)$expr
  if (is.character(x))
    x %>% constructor(envir)
  else {
    deparse(x) %>%
      paste(collapse = "\n") %>%
      stringr::str_replace_all("\\{\\\n\ +\\{", "{{") %>%
      stringr::str_replace_all("\\}\\\n\ +\\}", "}}") %>%
      constructor(envir)
  }

}

#' @export
as.function.template <- function(x, ...) {
  
  if (length(list(...)) > 0) {
    x <- update(x, ...)
  }
  
  templateAsFun(x, attr(x, "envir"))
  
}

#' @export
print.template <- function(x, ...) {
  cat(x, "\n")
  invisible(x)
}

#' @export
update.template <- function(object, ..., eval = TRUE) {
  if (eval) templateEval(object, ...)
  else templateSub(object, ...)
}

#' @export
update.function <- update.template
