
knitr::knit("vignettes/Template_Programming_in_R.Rmd", "Readme.md")


library("templates")

query <- "UPDATE {{ table }} SET col1 = {{ col1 }} WHERE primaryKey = {{ primaryKey }};"

dat <- data.frame(col1 = rnorm(10), primaryKey = 1:10)

dat::sac(dat, tmpl, "primaryKey", .t = query, table = "someTable")

