## Template-Programming in R



### Some ideas on working with templates in R

#### Functions:


```r
library("templates")
tFun <- templateFun(function() {
  s <- "great idea!!!"
  cat({{ toupper(begin) }}, s)
  invisible(NULL)
})

templateSub(tFun, begin ~ 'This is a')
```

```
## function () 
## {
##     s <- "great idea!!!"
##     cat(toupper("This is a"), s)
##     invisible(NULL)
## }
```

```r
templateEval(tFun, begin ~ 'This is a')
```

```
## function () 
## {
##     s <- "great idea!!!"
##     cat("THIS IS A", s)
##     invisible(NULL)
## }
```


#### Expressions:


```r
tExpr <- templateExpr(local({
  s <- "great idea!!!"
  cat({{ toupper(begin) }}, s)
}))

templateSub(tExpr, begin ~ 'This is a')
```

```
## local({
##     s <- "great idea!!!"
##     cat(toupper("This is a"), s)
## })
```

```
## local({
##     s <- "great idea!!!"
##     cat(toupper("This is a"), s)
## })
```

```r
templateEval(tExpr, begin ~ 'This is a')
```

```
## local({
##     s <- "great idea!!!"
##     cat("THIS IS A", s)
## })
```

```
## local({
##     s <- "great idea!!!"
##     cat("THIS IS A", s)
## })
```

```r
templateEvalHere(tExpr)
```

```
## Error in eval(expr, envir, enclos): could not find function "templateEvalHere"
```


#### Character:


```r
tChar <- templateChar('local({
  s <- "great idea!!!"
  cat({{ toupper(begin) }}, s)
})')

templateSub(tExpr, begin ~ 'This is a')
```

```
## local({
##     s <- "great idea!!!"
##     cat(toupper("This is a"), s)
## })
```

```
## local({
##     s <- "great idea!!!"
##     cat(toupper("This is a"), s)
## })
```

```r
templateEval(tExpr, begin ~ 'This is a')
```

```
## local({
##     s <- "great idea!!!"
##     cat("THIS IS A", s)
## })
```

```
## local({
##     s <- "great idea!!!"
##     cat("THIS IS A", s)
## })
```

```r
templateEvalHere(tExpr)
```

```
## Error in eval(expr, envir, enclos): could not find function "templateEvalHere"
```


### Some more usefull examples


#### MySQL Queries:


```r
templateRead <- function(fileName) {
  templateChar(paste(readLines(fileName), collapse = "\n"))
}

collapseInParan <- function(x) paste("(", paste(x, collapse = ", "), ")")

sqlTemplate <- templateRead(system.file("tmp.sql", package = "templates"))

sqlTemplate
```

```
## SELECT *
## FROM someTable
## WHERE something IN {{ ids }};
```

```
## SELECT *
## FROM someTable
## WHERE something IN {{ ids }};
```

```r
templateSub(
  sqlTemplate,
  ids = collapseInParan(1:10)
)
```

```
## SELECT *
## FROM someTable
## WHERE something IN ( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
```

```
## SELECT *
## FROM someTable
## WHERE something IN ( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
```

```r
sqlTemplate <- templateExpr(
  `SELECT *
    FROM someTable
  WHERE something IN {{ collapseInParan(ids) }};`
)

templateEval(
  sqlTemplate, 
  ids ~ 1:10
)
```

```
## SELECT *
##     FROM someTable
##   WHERE something IN ( 1:10 );
```

```
## SELECT *
##     FROM someTable
##   WHERE something IN ( 1:10 );
```

