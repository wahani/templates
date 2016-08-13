## Template-Programming in R



### Some examples

#### MySQL Queries:


```r
library("templates")
library("magrittr")

sqlTemplate <- template(
  `SELECT *
   FROM someTable
   WHERE something IN {{ collapseInParan(ids) }};`
)

collapseInParan <- function(x) {
  paste("(", paste(x, collapse = ", "), ")")
}

update(
  sqlTemplate, 
  ids = dput(1:10)
)
```

```
## 1:10
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

update(tFun, begin ~ "This is a")
```

```
## function () 
## {
##     s <- "great idea!!!"
##     cat("THIS IS A", s, "\n")
##     invisible(NULL)
## }
```

```r
update(tFun, begin ~ "This is a", eval = TRUE)
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
tExpr <- template({
  cat({{ toupper(begin) }}, "\n")
})

update(tExpr, begin ~ "hi", eval = TRUE)
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
tChar <- template('{
  cat({{ toupper(begin) }}, "\n")
}')

tChar %>%
  update(begin ~ "hi") %>%
  templateEvalHere
```

```
## HI
```
