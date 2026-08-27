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
    result_val <- mantel_fleiss_crit(grp, rsp, strata, TRUE)
  )

  expect_identical(result, TRUE)
  expect_identical(result_val, result, ignore_attr = TRUE)
  expect_equal(attributes(result_val), list(value = 20.16857), tolerance = 1e-6)
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
    result_val <- mantel_fleiss_crit(grp, rsp, strata, TRUE)
  )

  expect_identical(result, FALSE)
  expect_identical(result_val, result, ignore_attr = TRUE)
  expect_equal(attributes(result_val), list(value = 2.785714), tolerance = 1e-6)
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
    result_val <- mantel_fleiss_crit(grp, rsp, include_value = TRUE)
  )

  # Explicit stratum.
  expect_silent(
    result_1stratum <- mantel_fleiss_crit(grp, rsp, factor(rep("A", n)))
  )
  expect_silent(
    result_1stratum_det <- mantel_fleiss_crit(grp, rsp, factor(rep("A", n)), TRUE)
  )

  expect_identical(result, result_1stratum)
  expect_identical(result_val, result_1stratum_det)

  expect_identical(result, FALSE)
  expect_identical(result_val, result, ignore_attr = TRUE)
  expect_equal(attributes(result_val), list(value = 3.6), tolerance = 1e-6)
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
    result_val <- mantel_fleiss_crit(grp, rsp, strata, TRUE)
  )

  expect_identical(result, TRUE)
  expect_identical(result_val, result, ignore_attr = TRUE)
  expect_equal(attributes(result_val), list(value = 5.73951), tolerance = 1e-6)
})

test_that("mantel_fleiss_crit() handles a stratum with observations in one cell only", {
  grp <- factor(c("Act", "Act", "Cntrl", "Cntrl", "Act", "Act", "Act", "Act"))
  rsp <- c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
  strata <- factor(c(rep("A", 4), rep("B", 4)))

  result <- mantel_fleiss_crit(grp, rsp, strata)
  result_val <- mantel_fleiss_crit(grp, rsp, strata, TRUE)

  expect_identical(result, FALSE)
  expect_identical(result_val, result, ignore_attr = TRUE)
  expect_equal(attributes(result_val), list(value = 1), tolerance = 1e-6)
})

test_that("mantel_fleiss_crit() includes the MF = 5 boundary", {
  grp <- factor(c(rep("Active", 10), rep("Control", 20)))
  rsp <- c(rep(c(TRUE, FALSE), 5), rep(c(TRUE, FALSE), 10))

  result <- mantel_fleiss_crit(grp, rsp, include_value = TRUE)
  expect_identical(result, TRUE, ignore_attr = TRUE)
  expect_identical(attributes(result), list(value = 5))
})

test_that("mantel_fleiss_crit() handles only TRUE responses", {
  grp <- factor(rep(c("Active", "Control"), each = 5))
  rsp <- rep(TRUE, 10)
  strata <- factor(rep(c("A", "B"), each = 5))

  result <- mantel_fleiss_crit(grp, rsp, strata)
  result_val <- mantel_fleiss_crit(grp, rsp, strata, TRUE)

  expect_identical(result, FALSE)
  expect_identical(result_val, result, ignore_attr = TRUE)
  expect_identical(attributes(result_val), list(value = 0))
})

test_that("mantel_fleiss_crit() handles only FALSE responses", {
  grp <- factor(rep(c("Active", "Control"), each = 5))
  rsp <- rep(FALSE, 10)
  strata <- factor(rep(c("A", "B"), each = 5))

  result <- mantel_fleiss_crit(grp, rsp, strata)
  result_val <- mantel_fleiss_crit(grp, rsp, strata, TRUE)

  expect_identical(result, FALSE)
  expect_identical(result_val, result, ignore_attr = TRUE)
  expect_identical(attributes(result_val), list(value = 0))
})

test_that("mantel_fleiss_crit() works with observations from one group only", {
  grp <- factor(rep("Active", 8), levels = c("Active", "Control"))
  rsp <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  strata <- factor(rep(c("A", "B"), each = 4))

  result <- mantel_fleiss_crit(grp, rsp, strata)
  result_val <- mantel_fleiss_crit(grp, rsp, strata, TRUE)

  expect_identical(result, FALSE)
  expect_identical(result_val, result, ignore_attr = TRUE)
  expect_identical(attributes(result_val), list(value = 0))
})

test_that("mantel_fleiss_crit() validates inputs", {
  grp <- factor(c("G1", "G1", "G2", "G2"))
  rsp <- c(TRUE, FALSE, TRUE, FALSE)
  strata <- factor(c("A", "A", "B", "B"))

  # grp
  expect_error(mantel_fleiss_crit(factor(rep("G1", 4)), rsp))
  expect_error(mantel_fleiss_crit(factor(grp, levels = c("G1", "G2", "X")), rsp))
  expect_error(mantel_fleiss_crit(as.character(grp), rsp))
  expect_error(mantel_fleiss_crit(as.numeric(grp), rsp))
  expect_error(mantel_fleiss_crit(c(grp[-1], NA), rsp))
  # rsp
  expect_error(mantel_fleiss_crit(grp, as.character(rsp)))
  expect_error(mantel_fleiss_crit(grp, as.numeric(rsp)))
  expect_error(mantel_fleiss_crit(grp, c(rsp[-2], NA)))
  # strata
  expect_error(mantel_fleiss_crit(grp, rsp, as.character(strata)))
  expect_error(mantel_fleiss_crit(grp, rsp, as.numeric(strata)))
  expect_error(mantel_fleiss_crit(grp, rsp, c(strata[-3], NA)))
  # result_val
  expect_error(mantel_fleiss_crit(grp, rsp, strata, c(TRUE, FALSE)))

  # Different lengths.
  expect_error(mantel_fleiss_crit(grp[-1], rsp))
  expect_error(mantel_fleiss_crit(grp, rsp[-1]))
  expect_error(mantel_fleiss_crit(grp, rsp, strata[-2]))
})
