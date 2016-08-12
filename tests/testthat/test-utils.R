test_that("Template", {

  expectEqual <- function(a, b) testthat::expect_equal(a, b)

##############################################################################

  t1 <- template({
    return({{ 2 * a }})
  })

  expectEqual(
    as.function(
      t1,
      a ~ x * 2,
      eval = FALSE
    )(x = 1),
    4
  )

  expectEqual(
    as.function(
      t1
    )(a = 1),
    2
  )

  sqlTemplate <- template(
    `( {{ collapse(ids) }} )`
  )

  collapse <- function(x) paste(x, collapse = ", ")

  expectEqual(
    as.character(
      update(
        sqlTemplate,
        ids = 1:2,
        eval = TRUE
      )),
    "( 1, 2 )"
  )

  testthat::expect_warning(
    unclass(
      update(
        sqlTemplate,
        ids = 1:2,
        eval = FALSE
      )),
    "length = 1"
  )

  t2 <- function(x) {
    return({{ 2 * a }})
  }

  expectEqual(
    update(t2, a ~ x * 2, eval = FALSE)(2),
    8
  )

  expectEqual(
    local({
      a <- 2
      t2 <- function(x) {
        return({{ 2 * a }})
      }
      update(t2, eval = FALSE)(80)
    }),
    4
  )


##############################################################################

  t3 <- template(
    "{{ (function() {1})() }}
     {{ '\"HuHu\"' }}"
  )

  t3Eval <- template(
    "1
     \"HuHu\""
  )

  expectEqual(
    unclass(update(t3, eval = TRUE)),
    unclass(t3Eval)
  )

##############################################################################

  t4 <- template(
    "{{ a }}"
  )

  expectEqual(
    as.function(
      t4,
      a ~ 2,
      eval = FALSE
    )(),
    2
  )

  expectEqual(
    as.function(
      t4,
      a ~ x * 2,
      eval = FALSE
    )(x = 2),
    4
  )

  expectEqual(
    as.function(
      t4
    )(a = 1),
    1
  )

##############################################################################

  t5 <- template({
    x <- 2
  })

  t6 <- template({
    y <- 1
    y
  })

  templateEvalHere(t5)
  expectEqual(x, 2)
  expectEqual(templateEvalLocal(t6), 1)
  expectEqual(exists("y"), FALSE)
  
})
