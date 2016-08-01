test_that("Template", {

  expectEqual <- function(a, b) testthat::expect_equal(a, b)

  ##############################################################################

  t1 <- templateExpr({
    return({{ 2 * a }})
  })

  expectEqual(
    as.function(
      t1,
      a ~ x * 2
    )(x = 1),
    4
  )

  expectEqual(
    as.function(
      t1
    )(a = 1),
    2
  )

  sqlTemplate <- templateExpr(
    `{{ collapseInParan(ids) }}`
  )

  expectEqual(
    templateEval(
      sqlTemplate,
      ids ~ 1:2
    ),
    "( 1, 2 )"
  )


  t2 <- templateFun(function(x) {
    return({{ 2 * a }})
  })

  expectEqual(
    as.function(
      t2,
      a ~ x * 2
    )(2),
    8
  )

  expectEqual(
    local({
      a <- 2
      templateEval(t2)(80)
    }),
    4
  )


  ##############################################################################

  t3 <- templateChar(
    "{{ (function() {1})() }}
     {{ \"'HuHu'\" }}"
  )

  t3Eval <- templateChar(
    "1
     'HuHu'"
  )

  expectEqual(
    templateEval(t3),
    t3Eval
  )


  ##############################################################################

  t4 <- templateChar(
    "{{ a }}"
  )

  expectEqual(
    as.function(
      t4,
      a ~ 2
    )(),
    2
  )

  expectEqual(
    as.function(
      t4,
      a ~ x * 2
    )(x = 2),
    4
  )

  expectEqual(
    as.function(
      t4
    )(a = 1),
    1
  )

})
