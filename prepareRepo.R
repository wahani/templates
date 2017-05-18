
knitr::knit("vignettes/Template_Programming_in_R.Rmd", "Readme.md")

readme <- readLines("Readme.md")
readme <- readme[-(1:Position(function(x) x == "---", readme, TRUE))]

travis <- "[![Travis-CI Build Status](https://travis-ci.org/wahani/templates.svg?branch=master)](https://travis-ci.org/wahani/templates)"
codecov <- "[![Coverage Status](https://img.shields.io/codecov/c/github/wahani/templates/master.svg)](https://codecov.io/github/wahani/templates?branch=master)"
cran <- "[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/templates)](http://cran.r-project.org/package=templates)"
downloads <- "[![Downloads](http://cranlogs.r-pkg.org/badges/templates?color=brightgreen)](http://www.r-pkg.org/pkg/templates)"

writeLines(c(travis, codecov, cran, downloads, readme), "Readme.md")


library("templates")

