#' Util functions
#'
#' @param x (template)
#' @param ... dots
#' @param parent (environment)
#'
#' @rdname utils
#' @export
templateEval <- function(x, ...) UseMethod("templateEval")

#' @export
#' @rdname utils
templateEval.default <- function(x, ...) {

  evaluator <- function(x, envir, enclos) {
     flatmap(x, function(sexpr) {
      as.character(eval(parse(text = sexpr), envir = envir, enclos = enclos))
    }) 
  }

  templateUtility(x, ..., utility = evaluator)

}

#' @export
#' @rdname utils
templateEval.function <- function(x, ...) {

  newBody <- templateEval(
    eval(call("template", body(x))),
    ...,
    parent = attr(x, "envir")
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

  checkIntput <- function(x) {
    map(x, warnings$hasLengthOne)
    invisible(x)
  }

  substituter <- function(x, substitutes, ...) {

    subs <- map(substitutes, ~ parse(text = .))
    subs <- map(checkIntput(subs), ~ .[[1]])
    subCall <- as.call(
      c(quote(substitute), parse(text = x), quote(subs))
    )
    flatmap(x, x ~ deparse(eval(subCall)))
  }

  templateUtility(x, ..., utility = substituter)
  
}

templateUtility <- function(x, ..., utility) {

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

  template(ret, attr(x, "envir"))
  
}

#' @export
templateSub.function <- function(x, ...) {

  newBody <- templateSub(eval(call("template", body(x))), ...)
  body(x) <- parse(text = unclass(newBody))
  x
  
}

templateAsFun <- function(x, parent = parent.frame()) {

  force(x)
  force(parent)

  function(...) {
    eval(parse(text = x), envir = list(...), enclos = parent)
  }

}

#' @rdname utils
#' @export
templateEvalHere <- function(template, envir = parent.frame(), ...) {
  eval(parse(text = template), envir = envir, ...)
}

#' @rdname utils
#' @export
templateEvalLocal <- function(template, envir = parent.frame(), ...) {
  eval(parse(text = template), envir = new.env(parent = envir), ...)
}

