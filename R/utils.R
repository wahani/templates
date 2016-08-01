#' Util functions
#'
#' @param x (Template)
#' @param ... dots
#' @param parent (environment)
#'
#' @rdname utils
#' @export
templateEval <- function(x, ...) UseMethod("templateEval")

#' @export
#' @rdname utils
templateEval.default <- function(x, ..., parent = parent.frame()) {

  evaluator <- function(x, envir, enclos) {
     flatmap(x, function(sexpr) {
      as.character(eval(parse(text = sexpr), envir = envir, enclos = enclos))
    }) 
  }

  templateUtility(x, ..., parent = parent, utility = evaluator)

}

#' @export
#' @rdname utils
templateEval.templateFun <- function(x, ..., parent = parent.frame()) {

  newBody <- templateEval(
    eval(call("templateExpr", body(x))),
    ...,
    parent = parent
  )
  
  body(x) <- parse(text = unclass(newBody))
  x
  
}

#' @rdname utils
#' @export
templateSub <- function(x, ...) UseMethod("templateSub")

#' @rdname utils
#' @export
templateSub.default <- function(x, ...) {

  subsituter <- function(x, substitutes, ...) {
    map(names(substitutes) ~ substitutes ~ x, sub)
  }

  templateUtility(x, ..., utility = subsituter)
  
}


templateUtility <- function(x, ..., parent, utility) {

  rememberClass <- class(x)

  substitutes <- list(...)
  ind <- flatmap(substitutes, ~ inherits(., "formula"))

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
    utility(substitutes, parent)

  ret <- Reduce(x = replacements, init = x, function(acc, r) {
    stringr::str_replace(acc, getPattern(), r)
  })

  class(ret) <- rememberClass
  ret
  
}


#' @rdname utils
#' @export
templateSub.templateFun <- function(x, ...) {

  newBody <- templateSub(eval(call("templateExpr", body(x))), ...)
  body(x) <- parse(text = unclass(newBody))
  x
  
}

#' @rdname utils
#' @export
templateAsFun <- function(x, parent = parent.frame()) {

  force(x)
  force(parent)

  function(...) {
    eval(parse(text = x), envir = list(...), enclos = parent)
  }

}

addClass <- function (x, class) {
    class(x) <- unique(c(class, class(x)))
    x
}

getPattern <- function(case) "\\{\\{((?!\\{\\{)[\\\n[:print:]])*\\}\\}"

