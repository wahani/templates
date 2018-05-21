asCharacter <- function(...) {
  res <- as.character(...)
  if (length(res) == 0) "" # happens when '{{ null }}'
  else res
}
