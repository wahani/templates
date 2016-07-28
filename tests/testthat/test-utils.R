test_that("Template", {

  t1 <- Template("{{ (function() {1})() }}
                {{ 'HuHu' }}")

  t2 <- Template("{{ testFunction('HuHu') }}
                {{ mean(1:10) }}")

  t3 <- Template("x <- 5
               x * {{ arg }}")

  testFunction <- function(x) x

  asCharacter(t1)
  asCharacter(t2)
  asFunction(t3)(arg = mean(1:10))

})

