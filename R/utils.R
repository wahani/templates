#' Util functions
#'
#' @param x (template)
#' @param ... dots
#' @param envir (environment)
#'
#' @rdname utils
#' @export
tmplUpdate <- function(x, ...) UseMethod("tmplUpdate")

#' @export
#' @rdname utils
tmplUpdate.default <- function(x, ...) {

  evaluator <- function(x, envir, enclos) {
     flatmap(x, function(sexpr) {
      as.character(eval(parse(text = sexpr), envir = envir, enclos = enclos))
    }) 
  }

  tmplUtility(x, ..., utility = evaluator)

}

#' @export
#' @rdname utils
tmplUpdate.function <- function(x, ...) {

  newBody <- as.formula(paste("~", paste(deparse(body(x)), collapse = "\n")))
  newBody <- tmplUpdate(tmpl(newBody, envir = environment(x)), ...)

  body(x) <- parse(text = unclass(newBody))
  x
  
}

tmplUtility <- function(x, ..., utility) {

  substitutes <- list(...)
  ind <- flatmap(substitutes, inherits , "formula")
  
  if (sum(ind) > 0) {
    subtExpr <- extract(substitutes, ind) %>% flatmap(f ~ deparse(f[[2]]))
    exprList <- extract(substitutes, ind) %>% flatmap(f ~ deparse(f[[3]]))
    substitutes <- replace(substitutes, ind, exprList)
    names(substitutes)[ind] <- subtExpr
  }

  replacements <-
    stringr::str_extract_all(x, pattern = getPattern()) %>%
    unlist %>%
    stringr::str_replace_all("(^\\{\\{)|(\\}\\}$)", "") %>%
    stringr::str_trim() %>%
    utility(substitutes, attr(x, "envir"))

  ret <- Reduce(x = replacements, init = x, function(acc, r) {
    stringr::str_replace(acc, getPattern(), r)
  })

  tmpl(ret, envir = attr(x, "envir"))
  
}

#' @rdname utils
#' @export
tmplEval <- function(x, envir = new.env(parent = parent.frame()), ...) {
  template <- tmplUpdate(x, ...)
  eval(parse(text = template), envir = envir, enclos = attr(x, "envir"))
}
