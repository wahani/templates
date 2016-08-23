## Template-Programming in R



### Some examples

#### MySQL Queries:


```r
library("templates")
library("magrittr")

sqlTemplate <- tmpl(
  ~ `SELECT *
   FROM someTable
   WHERE something IN {{ collapseInParan(ids) }};`
)

collapseInParan <- function(x) {
  paste("(", paste(x, collapse = ", "), ")")
}

tmpl(
  sqlTemplate, 
  ids = 1:10
)
```

```
## SELECT *
##    FROM someTable
##    WHERE something IN ( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
```


### Some ideas on working with templates in R

#### Functions:


```r
tFun <- function() {
  s <- "great idea!!!"
  cat({{ toupper(begin) }}, s, "\n")
  invisible(NULL)
}

tmpl(tFun, begin ~ "This is a")
```

```
## function () 
## {
##     s <- "great idea!!!"
##     cat("THIS IS A", s, "\n")
##     invisible(NULL)
## }
```


#### Expressions:


```r
tExpr <- tmpl( ~ {
  cat({{ toupper(begin) }}, "\n")
})

tmpl(tExpr, begin ~ "hi")
```

```
## {
##     cat("HI", "\n")
## }
```

```r
as.function(tExpr, begin ~ "hi")()
```

```
## HI
```


#### Character:


```r
tChar <- tmpl('{
  cat({{ toupper(begin) }}, "\n")
}')

tChar %>%
  tmpl(begin ~ "hi") %>%
  tmplEval
```

```
## HI
```
