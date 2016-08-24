
knitr::knit("vignettes/Template_Programming_in_R.Rmd", "Readme.md")

readme <- readLines("Readme.md")
readme <- readme[-(1:Position(function(x) x == "---", readme, TRUE))]

travis <- "[![Travis-CI Build Status](https://travis-ci.org/wahani/templates.svg?branch=master)](https://travis-ci.org/wahani/templates)"
codecov <- "[![Coverage Status](https://img.shields.io/codecov/c/github/wahani/templates/master.svg)](https://codecov.io/github/wahani/templates?branch=master)"

writeLines(c(travis, codecov, readme), "Readme.md")

library("templates")

query <- "UPDATE {{ table }} SET col1 = {{ col1 }} WHERE primaryKey = {{ primaryKey }};"

dat <- data.frame(col1 = rnorm(10), primaryKey = 1:10)

dat::sac(dat, tmpl, "primaryKey", .t = query, table = "someTable")

