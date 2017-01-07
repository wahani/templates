#' Template constructors
#'
#' \code{tmpl} is the constructor function for template objects. 
#'
#' @param .t something that can be interpreted as template. See defined methods
#'   for options.
#' @param .envir (environment) the environment in which template snippets are
#'   evaluated. For \code{formula}s and \code{function}s their environment is
#'   used.
#' @param ... (name = value | name ~ value) name-value expressions passed on to
#'   \link{tmplUpdate}
#'
#' @details Objects of class \code{tmpl} are stored as a character of length
#'   one. They can contain 'snippets' to be evaluated. These snippets are
#'   identified by an opening \code{\{\{} and closing \code{\}\}}. The
#'   environment in which they are evaluated is stored in the object. They can
#'   be further augmented by supplying arguments in \code{...}.
#'
#' @seealso \link{tmplUpdate}, \link{tmplEval}
#'
#' @examples
#'
#' tmpl("Hi {{ toupper(a) }}!", a = "there")
#' tmpl( ~ {y <- {{ a }}}, a ~ x + 1)
#' tmpl(function(x) {{ a }} + x, a ~ 1)
#'
#' @export
#' @rdname tmpl
tmpl <- function(.t, ...) UseMethod("tmpl")

#' @export
#' @rdname tmpl
tmpl.character <- function(.t, ..., .envir = parent.frame()) {

  out <- tmplConstructor(.t, .envir)

  if (length(list(...)) == 0) out
  else tmplUpdate(out, ...)
  
}

tmplConstructor <- function(.t, .envir) {
  addAttr(.t, class = "tmpl", envir = .envir)
}

#' @export
#' @rdname tmpl
tmpl.formula <- function(.t, ...) {

  out <- .t[[2]] %>%
    deparse() %>%
    paste(collapse = "\n") %>%
    stringr::str_replace_all("\\{\\\n\ *\\{", "{{") %>%
    stringr::str_replace_all("\ *\\}\\\n\ *\\}", "}}")
  
  tmpl.character(out, ..., .envir = environment(.t))
  
}

#' @export
#' @rdname tmpl
tmpl.tmpl <- function(.t, ...) {
  tmplUpdate(.t, ...)
}

#' @export
#' @rdname tmpl
tmpl.function <- function(.t, ...) {
  tmplUpdate(.t, ...)
}

#' @export
print.tmpl <- function(x, ...) {
  cat(x, "\n")
  invisible(x)
}
