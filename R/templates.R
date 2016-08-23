#' Templates constructors
#'
#' @param x (character | expression)
#' @param envir (environment)
#' @param ... (name = value | name ~ value) name-value expressions
#'
#' @export
#' @rdname templates
tmpl <- function(x, ..., envir = parent.frame()) {

  constructor <- function(x, envir) {
    addAttr(x, class = "template", envir = envir)
  }

  if (is(x, "character"))
    out <- constructor(x, envir)
  else if (is(x, "formula")) {
    out <- x[[2]] %>%
      deparse() %>%
      paste(collapse = "\n") %>%
      stringr::str_replace_all("\\{\\\n\ +\\{", "{{") %>%
      stringr::str_replace_all("\ *\\}\\\n\ *\\}", "}}") %>%
      constructor(envir)
  }
  else if (is(x, "template"))
    out <- x
  else if (is(x, "function"))
    out <- x
  else stop("can't handle input")

  if (length(list(...)) == 0) out
  else tmplUpdate(out, ...)

}

#' @export
as.function.template <- function(x, ..., parent = attr(x, "envir")) {

  x <- tmplUpdate(x, ...)

  force(x)
  force(parent)

  function(...) {
    eval(parse(text = x), envir = list(...), enclos = parent)
  }
    
}

#' @export
print.template <- function(x, ...) {
  cat(x, "\n")
  invisible(x)
}
