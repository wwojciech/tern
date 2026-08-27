test_that("mantel_fleiss_crit() works with multiple observations and strata", {
  set.seed(123)
  n <- 100
  grp <- factor(sample(c("Active", "Control"), n, replace = TRUE))
  rsp <- sample(c(TRUE, FALSE), n, replace = TRUE)
  strata <- factor(sample(LETTERS[1:4], n, replace = TRUE))

  expect_silent(
    result <- mantel_fleiss_crit(grp, rsp, strata)
  )
  expect_silent(
    result_det <- mantel_fleiss_crit(grp, rsp, strata, TRUE)
  )

  expect_identical(result, TRUE)
  expect_identical(result_det, result, ignore_attr = TRUE)
  expect_equal(
    attributes(result_det),
    list(value = 20.16857, criterion = "MF >= 5"),
    tolerance = 1e-6
  )
})

test_that("mantel_fleiss_crit() works with small stratified data", {
  set.seed(123)
  n <- 20
  grp <- factor(sample(c("Active", "Control"), n, replace = TRUE))
  rsp <- sample(c(TRUE, FALSE), n, replace = TRUE)
  strata <- factor(sample(LETTERS[1:4], n, replace = TRUE))

  expect_silent(
    result <- mantel_fleiss_crit(grp, rsp, strata)
  )
  expect_silent(
    result_det <- mantel_fleiss_crit(grp, rsp, strata, TRUE)
  )

  expect_identical(result, FALSE)
  expect_identical(result_det, result, ignore_attr = TRUE)
  expect_equal(
    attributes(result_det),
    list(value = 2.785714, criterion = "MF >= 5"),
    tolerance = 1e-6
  )
})

test_that("mantel_fleiss_crit() works without strata", {
  set.seed(123)
  n <- 20
  grp <- factor(sample(c("Active", "Control"), n, replace = TRUE))
  rsp <- sample(c(TRUE, FALSE), n, replace = TRUE)

  # No explicit strata.
  expect_silent(
    result <- mantel_fleiss_crit(grp, rsp)
  )
  expect_silent(
    result_det <- mantel_fleiss_crit(grp, rsp, details = TRUE)
  )

  # Explicit stratum.
  expect_silent(
    result_1stratum <- mantel_fleiss_crit(grp, rsp, factor(rep("A", n)))
  )
  expect_silent(
    result_1stratum_det <- mantel_fleiss_crit(grp, rsp, factor(rep("A", n)), TRUE)
  )

  expect_identical(result, result_1stratum)
  expect_identical(result_det, result_1stratum_det)

  expect_identical(result, FALSE)
  expect_identical(result_det, result, ignore_attr = TRUE)
  expect_equal(
    attributes(result_det),
    list(value = 3.6, criterion = "MF >= 5"),
    tolerance = 1e-6
  )
})

test_that("mantel_fleiss_crit() ignores unused strata levels", {
  set.seed(123)
  n <- 40

  grp <- factor(sample(c("Active", "Control"), n, replace = TRUE))
  rsp <- sample(c(TRUE, FALSE), n, replace = TRUE)
  strata <- factor(
    sample(LETTERS[1:3], n, replace = TRUE),
    levels = LETTERS[1:4]
  )

  expect_silent(
    result <- mantel_fleiss_crit(grp, rsp, strata)
  )
  expect_silent(
    result_det <- mantel_fleiss_crit(grp, rsp, strata, TRUE)
  )

  expect_identical(result, TRUE)
  expect_identical(result_det, result, ignore_attr = TRUE)
  expect_equal(
    attributes(result_det),
    list(value = 5.73951, criterion = "MF >= 5"),
    tolerance = 1e-6
  )
})

test_that("mantel_fleiss_crit() handles a stratum with observations in one cell only", {
  grp <- factor(c("Act", "Act", "Cntrl", "Cntrl", "Act", "Act", "Act", "Act"))
  rsp <- c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
  strata <- factor(c(rep("A", 4), rep("B", 4)))

  result <- mantel_fleiss_crit(grp, rsp, strata, details = TRUE)

  expect_silent(
    result <- mantel_fleiss_crit(grp, rsp, strata)
  )
  expect_silent(
    result_det <- mantel_fleiss_crit(grp, rsp, strata, TRUE)
  )

  expect_identical(result, FALSE)
  expect_identical(result_det, result, ignore_attr = TRUE)
  expect_equal(
    attributes(result_det),
    list(value = 1, criterion = "MF >= 5"),
    tolerance = 1e-6
  )
})
