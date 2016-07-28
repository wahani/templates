## Template-Programming in R




### Some ideas on working with MySQL Queries


```r
library("templates")

# Some helpers:
collapseInParan <- function(x) paste("(", paste(x, collapse = ", "), ")")

readFile <- function(fileName) {
  paste(readLines(fileName), collapse = "\n")
}

deparseQuery <- function(query) {
  deparse(substitute(query))
}

# A sql-query
sqlTemplate <- readFile(system.file("tmp.sql", package = "templates"))

cat(sqlTemplate)
```

```
## SELECT *
## FROM someTable
## WHERE something IN {{ ids }};
```

```r
cat(asCharacter(
  sqlTemplate,
  ids = collapseInParan(1:10)
))
```

```
## SELECT *
## FROM someTable
## WHERE something IN ( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
```

```r
query <- deparseQuery(
  `SELECT *
    FROM someTable
  WHERE something IN {{ ids }};`
)

cat(asCharacter(
  query,
  ids = collapseInParan(1:10)
))
```

```
## SELECT *
##     FROM someTable
##   WHERE something IN ( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
```


### Some ideas on working with functions


```r
funTemplate <- Template("x <- {{ arg }}
                         x * 2")

asFunction(funTemplate)(arg = 2)
```

```
## [1] 4
```

