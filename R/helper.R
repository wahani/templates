asCharacter <- function(...) {
  res <- as.character(...)
  if (length(res) == 0) "" # happens when '{{ null }}'
  else res
}

addAttr <- function(x, ...) {
  newAttr <- list(...)
  for (n in names(newAttr)) {
    attr(x, n) <- extract2(newAttr, n)
  }
  x
}

getPattern <- function(case) "\\{\\{((?!\\{\\{)[\\\n[:print:]])*\\}\\}"
