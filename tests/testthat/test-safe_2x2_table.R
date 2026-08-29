test_that("safe_2x2_table() works without strata", {
  set.seed(123)
  n <- 100
  rsp <- sample(c(TRUE, FALSE), n, replace = TRUE)
  grp <- factor(sample(c("Placebo", "X"), n, replace = TRUE))

  expect_silent(
    result <- safe_2x2_table(rsp, grp)
  )
  expected <- as.table(array(
    c(26L, 31L, 20L, 23L),
    dim = c(2L, 2L),
    dimnames = list(grp = c("Placebo", "X"), rsp = c("TRUE", "FALSE"))
  ))

  expect_identical(result, expected)
})

test_that("safe_2x2_table() works with multiple observations and strata", {
  set.seed(123)
  n <- 100
  rsp <- sample(c(TRUE, FALSE), n, replace = TRUE)
  grp <- factor(sample(c("Placebo", "X"), n, replace = TRUE))
  strata <- factor(sample(LETTERS[1:4], n, replace = TRUE))

  expect_silent(
    result <- safe_2x2_table(rsp, grp, strata)
  )
  expected <- as.table(array(
    c(6L, 9L, 9L, 8L, 8L, 6L, 5L, 5L, 5L, 5L, 4L, 5L, 7L, 11L, 2L, 5L),
    dim = c(2, 2, 4),
    dimnames = list(grp = c("Placebo", "X"), rsp = c("TRUE", "FALSE"), strata = LETTERS[1:4])
  ))

  expect_identical(result, expected)
})

test_that("safe_2x2_table() gives the same result with one stratum", {
  set.seed(123)
  n <- 20
  grp <- factor(sample(c("Active", "Control"), n, replace = TRUE))
  rsp <- sample(c(TRUE, FALSE), n, replace = TRUE)
  strata <- factor(rep("A", n))

  expect_silent(
    result <- safe_2x2_table(rsp, grp)
  )
  expect_silent(
    result_1stratum <- safe_2x2_table(rsp, grp, strata)
  )

  expect_identical(result, result_1stratum[, , 1])
})

test_that("safe_2x2_table() retains unobserved response outcomes (TRUE only)", {
  rsp <- rep(TRUE, 4)
  grp <- factor(c("Placebo", "Placebo", "X", "X"))
  strata <- factor(c("S1", "S2", "S1", "S2"))

  expect_silent(
    result <- safe_2x2_table(rsp, grp)
  )
  expect_silent(
    result_strata <- safe_2x2_table(rsp, grp, strata)
  )

  dimnames <- list(grp = c("Placebo", "X"), rsp = c("TRUE", "FALSE"))
  expected <- as.table(array(c(2L, 2L, 0L, 0L), dim = c(2L, 2L), dimnames = dimnames))
  expected_strata <- as.table(array(
    c(1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L),
    dim = c(2L, 2L, 2L),
    dimnames = c(dimnames, strata = list(c("S1", "S2")))
  ))

  expect_identical(result, expected)
  expect_identical(result_strata, expected_strata)
})

test_that("safe_2x2_table() retains unobserved response outcomes (FALSE only)", {
  rsp <- rep(FALSE, 4)
  grp <- factor(c("Placebo", "Placebo", "X", "X"))
  strata <- factor(c("S1", "S2", "S1", "S2"))

  expect_silent(
    result <- safe_2x2_table(rsp, grp)
  )
  expect_silent(
    result_strata <- safe_2x2_table(rsp, grp, strata)
  )

  dimnames <- list(grp = c("Placebo", "X"), rsp = c("TRUE", "FALSE"))
  expected <- as.table(array(c(0L, 0L, 2L, 2L), dim = c(2L, 2L), dimnames = dimnames))
  expected_strata <- as.table(array(
    c(0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L),
    dim = c(2L, 2L, 2L),
    dimnames = c(dimnames, strata = list(c("S1", "S2")))
  ))

  expect_identical(result, expected)
  expect_identical(result_strata, expected_strata)
})

test_that("safe_2x2_table() retains unobserved group levels", {
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  grp <- factor(rep("X", 4), levels = c("Placebo", "X"))
  strata <- factor(c("S1", "S2", "S2", "S1"))

  expect_silent(
    result <- safe_2x2_table(rsp, grp)
  )
  expect_silent(
    result_strata <- safe_2x2_table(rsp, grp, strata)
  )

  dimnames <- list(grp = c("Placebo", "X"), rsp = c("TRUE", "FALSE"))
  expected <- as.table(array(c(0L, 2L, 0L, 2L), dim = c(2L, 2L), dimnames = dimnames))
  expected_strata <- as.table(array(
    c(0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L),
    dim = c(2L, 2L, 2L),
    dimnames = c(dimnames, strata = list(c("S1", "S2")))
  ))

  expect_identical(result, expected)
  expect_identical(result_strata, expected_strata)
})

test_that("safe_2x2_table() retains unused strata levels", {
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  grp <- factor(c("X", "X", "Placebo", "Placebo"))
  strata <- factor(c("A", "A", "B", "B"), levels = c("A", "B", "Z"))

  expect_silent(
    result <- safe_2x2_table(rsp, grp, strata)
  )

  expected <- as.table(array(
    c(0L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L),
    dim = c(2L, 2L, 3L),
    dimnames = list(grp = c("Placebo", "X"), rsp = c("TRUE", "FALSE"), strata = c("A", "B", "Z"))
  ))

  expect_identical(result, expected)
})

test_that("safe_2x2_table() handles sparse contingency tables", {
  rsp <- c(TRUE, TRUE, TRUE, TRUE)
  grp <- factor(c("Y", "Y", "Cntrl", "Cntrl"))
  strata <- factor(c("A", "A", "B", "B"))

  expect_silent(
    result <- safe_2x2_table(rsp, grp, strata)
  )

  expected <- as.table(array(
    c(0L, 2L, 0L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    dim = c(2L, 2L, 2L),
    dimnames = list(grp = c("Cntrl", "Y"), rsp = c("TRUE", "FALSE"), strata = c("A", "B"))
  ))

  expect_identical(result, expected)
})

test_that("safe_2x2_table() handles empty data with an unused stratum level", {
  grp <- factor(levels = c("Placebo", "X"))
  strata <- factor(levels = "S1")

  expect_silent(
    result <- safe_2x2_table(logical(), grp)
  )
  expect_silent(
    result_strata <- safe_2x2_table(logical(), grp, strata)
  )

  dimnames <- list(grp = c("Placebo", "X"), rsp = c("TRUE", "FALSE"))
  expected <- as.table(array(c(0L, 0L, 0L, 0L), dim = c(2L, 2L), dimnames = dimnames))
  expected_strata <- as.table(array(
    c(0L, 0L, 0L, 0L),
    dim = c(2L, 2L, 1L),
    dimnames = c(dimnames, strata = "S1")
  ))

  expect_identical(result, expected)
  expect_identical(result_strata, expected_strata)
})

test_that("safe_2x2_table() handles empty data with no stratum levels", {
  expect_silent(
    result <- safe_2x2_table(
      logical(), factor(levels = c("Placebo", "X")), factor()
    )
  )

  dimnames <- list(grp = c("Placebo", "X"), rsp = c("TRUE", "FALSE"), strata = character())
  expected <- as.table(array(integer(), dim = c(2L, 2L, 0L), dimnames = dimnames))

  expect_identical(result, expected)
})

test_that("safe_2x2_table() handles empty data without strata", {
  expect_silent(
    result <- safe_2x2_table(logical(), factor(levels = c("Placebo", "X")))
  )

  dimnames <- list(grp = c("Placebo", "X"), rsp = c("TRUE", "FALSE"))
  expected <- as.table(array(rep(0L, 4), dim = c(2L, 2L), dimnames = dimnames))

  expect_identical(result, expected)
})

test_that("safe_2x2_table() validates inputs (rsp)", {
  grp <- factor(c("G1", "G1", "G2", "G2"))
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  strata <- factor(c("A", "A", "B", "B"))

  expect_error(safe_2x2_table(as.character(rsp), grp))
  expect_error(safe_2x2_table(as.numeric(rsp), grp))
  expect_error(safe_2x2_table(c(rsp[-1], NA), grp))
})

test_that("safe_2x2_table() validates inputs (grp)", {
  grp <- factor(c("G1", "G1", "G2", "G2"))
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  strata <- factor(c("A", "A", "B", "B"))

  expect_error(safe_2x2_table(rsp, as.character(grp)))
  expect_error(safe_2x2_table(rsp, as.numeric(grp)))
  expect_error(safe_2x2_table(rsp, factor(rep("G1", 4))))
  expect_error(safe_2x2_table(rsp, factor(grp, levels = c("G1", "G2", "G3"))))
  expect_error(safe_2x2_table(rsp, grp[-1]))
  expect_error(safe_2x2_table(rsp, factor(c(NA, grp[-1]))))
})

test_that("safe_2x2_table() validates inputs (strata)", {
  grp <- factor(c("G1", "G1", "G2", "G2"))
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  strata <- factor(c("A", "A", "B", "B"))

  expect_error(safe_2x2_table(rsp, grp, as.character(strata)))
  expect_error(safe_2x2_table(rsp, grp, as.numeric(strata)))
  expect_error(safe_2x2_table(rsp, grp, strata[-1]))
  expect_error(safe_2x2_table(rsp, grp, factor(c(strata[-1], NA))))
})
