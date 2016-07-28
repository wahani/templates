#' Util functions
#'
#' @param x (Template)
#' @param ... dots
#' @param parent (environment)
#'
#' @rdname utils
#' @export
asCharacter <- function(x, ..., parent = parent.frame()) {

  replacements <-
    stringr::str_extract_all(x, pattern = "\\{\\{.*\\}\\}") %>%
    unlist %>%
    stringr::str_replace_all("(^\\{\\{)|(\\}\\}$)", "") %>%
    stringr::str_trim() %>%
    flatmap(function(sexpr) {
      as.character(eval(parse(text = sexpr), envir = list(...), enclos = parent))
    })

  Reduce(x = replacements, init = x, function(acc, r) {
    stringr::str_replace(acc, "\\{\\{.*\\}\\}", r)
  })

}


#' @rdname utils
#' @export
asFunction <- function(x, parent = parent.frame()) {

  force(x)
  force(parent)

  function(...) {
    eval(parse(text = asCharacter(x, ..., parent)))
  }

}

