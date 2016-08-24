addAttr <- function(x, ...) {
  newAttr <- list(...)
  for (n in names(newAttr)) {
    attr(x, n) <- extract2(newAttr, n)
  }
  x
}

getPattern <- function(case) "\\{\\{((?!\\{\\{)[\\\n[:print:]])*\\}\\}"
