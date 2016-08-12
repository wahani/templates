addClass <- function (x, class) {
    class(x) <- unique(c(class, class(x)))
    x
}

addAttr <- function(x, ...) {
  newAttr <- list(...)
  for (n in names(newAttr)) {
    attr(x, n) <- extract2(newAttr, n)
  }
  x
}

getPattern <- function(case) "\\{\\{((?!\\{\\{)[\\\n[:print:]])*\\}\\}"
