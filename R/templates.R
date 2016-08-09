#' Templates constructors
#'
#' @param x
#'
#' @export
#' @rdname templates
template <- function(x) {

  x <- substitute(x)
  if (is.character(x))
    x %>% addClass("template")
  else {
    deparse(x) %>%
      stringr::str_replace_all("^[\\\"\\\']|[\\\"\\\']$", "") %>%
      paste(collapse = "\n") %>%
      stringr::str_replace_all("\\{\\\n\ +\\{", "{{") %>%
      stringr::str_replace_all("\\}\\\n\ +\\}", "}}") %>%
      addClass("template")                            
  }

}

#' @export
as.function.template <- function(x, ..., parent = parent.frame()) {
  
  if (length(list(...)) > 0) {
    x <- templateSub(x, ...)
  }    
  
  templateAsFun(x, parent)
  
}

#' @export
print.template <- function(x, ...) {
  cat(x, "\n")
  invisible(x)
}

#' @export
update.template <- function(object, ..., eval = FALSE) {
  if (eval) templateEval(object, ...)
  else templateSub(object, ...)
}

#' @export
update.function <- update.template
