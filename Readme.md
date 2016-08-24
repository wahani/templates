[![Travis-CI Build Status](https://travis-ci.org/wahani/templates.svg?branch=master)](https://travis-ci.org/wahani/templates)
[![Coverage Status](https://img.shields.io/codecov/c/github/wahani/templates/master.svg)](https://codecov.io/github/wahani/templates?branch=master)


## Template-Programming in R

Provides tools to work with template code and text in R. It aims to provide a simple substitutions mechanism for R-expressions inside these templates. Templates can be written in other languages like 'MySQL', can simply be represented by characters in R, or can themselves be R-expressions or functions.


## Installation


```r
devtools::install_github("wahani/templates")
```


## Why should you care?

- Probably you shouldn't
- This is the low level implementation
    - to reuse shinys reactives
    - to have parameterized sql-queries in `.sql` files
    - to do more meta-programming


## Some examples

### MySQL Queries:

Actually this package does not aim at providing parameterized sql-like queries;
but it implements the core idea behind it. Here we can use R-snippets inside
expressions, characters, or functions to inject code or rather text:


```r
library("templates")
library("magrittr")

sqlTemplate <- tmpl(
  ~ `SELECT *
   FROM someTable
   WHERE something IN {{ collapseInParan(ids) }};`
)

collapseInParan <- function(x) {
  # just a helper function
  paste("(", paste(x, collapse = ", "), ")")
}

tmplUpdate(
  sqlTemplate, 
  ids = 1:10
)
```

```
## SELECT *
##    FROM someTable
##    WHERE something IN ( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
```

The double `{` denote a region to be evaluated. They can contain arbitrary
R-code.


### Functions:

This may be useful to inject code into functions. For example to minimize the
need to query a database for simple requests when other options - like closures -
are not feasible.


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


### Expressions:

This might be helpful whenever we need to reuse 'code' where the environment
where it is evaluated has special meaning. This, for example, can be used to
implement parameterized reactive expressions in a shiny app.


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
tmplAsFun(tExpr, begin ~ "hi")()
```

```
## HI
```

```r
tmplEval(tExpr, begin ~ "hi")
```

```
## HI
```


### Character:

The leading example of using characters as template are parameterized sql
queries. Like any other template they can represent also R-code and then later
be evaluated.


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
