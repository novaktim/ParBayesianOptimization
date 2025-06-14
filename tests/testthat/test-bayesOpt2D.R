test_that(

  "2 Dimension"

  , {

    skip_on_cran()

    set.seed(1991)

    sf <- function(x,y) 1000 - (x-5)^2 - (y + 10)^2

    FUN <- function(x,y) {
      return(list(Score = sf(x,y)))
    }

    bounds = list(
        x = c(0,15)
      , y = c(-20,100)
    )

    optObj <- bayesOpt(
        FUN
      , bounds
      , initPoints = 4
      , iters.n = 2
      , verbose = 0
    )

    expect_true(optObj$stopStatus == "OK")
    expect_true(nrow(optObj$scoreSummary) == 6)

    optObj <- addIterations(
      optObj
      , iters.n = 2
      , verbose = 0
      , gsPoints = 10
    )

    optObj <- addIterations(
      optObj
      , iters.n = 2
      , iters.k = 2
      , verbose = 0
      , gsPoints = 10
    )


    # Piggy back off of this test. Check new bounds.
    newBounds <- list(
      x = c(-5,20)
      , y = c(-30,110)
    )

    optObj <- addIterations(
      optObj
      , bounds = newBounds
      , iters.n = 2
      , iters.k = 2
      , verbose = 0
      , gsPoints = 10
    )

    expect_true(nrow(optObj$scoreSummary) == 12)

  }
)
