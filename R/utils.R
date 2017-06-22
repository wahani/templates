#' Update and evaluate templates
#'
#' Functions operating on \link{tmpl} objects. They can be updated and / or
#' evaluated as an expression.
#' 
#' @param .t (tmpl) and object of class \code{tmpl}
#' @param ... (name = value | name ~ value) name-value expressions used to
#'   update the snippets in \code{x}
#' @param .envir (environment) the environment in which the template is
#'   evaluated
#'
#' @details
#' 
#' \code{tmplUpdate} will evaluate all snippets in a template. Objects are
#' searched for in the list of arguments supplied as \code{...} and the
#' environment of the template. The results are substituted with the snippets.
#'
#' \code{tmplEval} will evaluate the template in place or in the specified
#' environment after substituting the elements in \code{...}.
#' 
#' @examples
#' tmpl("This is {{ a }} very similar to {{ b }}", a = "actually", b = "sprintf")
#' tmpl("But I consider it to be ({{ sprintf('%i', a) }}) orthogonal", a = 1.0)
#' tmpl("and ({{ sprintf('%i', b) }}) with a different scope:", b = 2.0)
#' tmpl("SELECT {{ var }} FROM {{ table }} WHERE {{ condition }};",
#'      var = "someVar", table = "someTable", condition = "primaryKey = 1")
#' template <- tmpl("cat({{ toupper(x) }})")
#' tmplUpdate(template, x ~ "hi")
#' tmplEval(template, x ~ "hi")
#' 
#' @rdname utils
#' @export
tmplUpdate <- function(.t, ...) UseMethod("tmplUpdate")

#' @export
#' @rdname utils
tmplUpdate.tmpl <- function(.t, ...) {

  evaluator <- function(x, envir, enclos) {
    flatmap(x, function(sexpr) {
      asCharacter(eval(parse(text = sexpr), envir = envir, enclos = enclos))
    })
  }

  tmplUtility(.t, ..., .utility = evaluator)

}

#' @export
#' @rdname utils
tmplUpdate.function <- function(.t, ...) {

  newBody <- as.formula(paste("~", paste(deparse(body(.t)), collapse = "\n")))
  newBody <- tmpl(newBody, ...)

  body(.t) <- parse(text = unclass(newBody))
  .t
  
}

tmplUtility <- function(.t, ..., .utility) {

  substitutes <- list(...)

  ind <- flatmap(substitutes, is.list)
  if (any(ind)) {
    substitutes <- unlist(substitutes, recursive = FALSE)
  }
  
  ind <- flatmap(substitutes, inherits, what = "formula")
  if (any(ind)) {
    subtExpr <- extract(substitutes, ind) %>% flatmap(f ~ deparse(f[[2]]))
    exprList <- extract(substitutes, ind) %>% flatmap(f ~ deparse(f[[3]]))
    substitutes <- replace(substitutes, ind, exprList)
    names(substitutes)[ind] <- subtExpr
  }

  replacements <-
    stringr::str_extract_all(.t, pattern = getPattern()) %>%
    unlist %>%
    stringr::str_replace_all("(^\\{\\{)|(\\}\\}$)", "") %>%
    stringr::str_trim() %>%
    .utility(substitutes, attr(.t, "envir"))

  ret <- Reduce(x = replacements, init = .t, function(acc, r) {
    stringr::str_replace(acc, getPattern(), r)
  })

  tmplConstructor(ret, .envir = attr(.t, "envir"))
  
}

#' @rdname utils
#' @export
tmplEval <- function(.t, ..., .envir = new.env(parent = parent.frame())) {
  template <- tmplUpdate(.t, ...)
  eval(parse(text = template), envir = .envir, enclos = attr(.t, "envir"))
}

#' @rdname utils
#' @export
tmplAsFun <- function(.t, ...) {

  .t <- tmplUpdate(.t, ...)

  function(...) {
    eval(parse(text = .t), envir = list(...), enclos = attr(.t, "envir"))
  }
    
}
