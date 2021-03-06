context("'kaya' function - correct calculation")

test_that("Correct calculations for emmissions", {
  # CO2
  expect_true(identical(kaya(pop = 2, gdp = 3, enInt = 4, carbInt = 5), 120))
  expect_true(identical(kaya(pop = 4, gdp = 3, enInt = 2, carbInt = 1), 24))
  expect_true(identical(kaya(pop = 0, gdp = 3, enInt = 2, carbInt = 1), 0))

  # C
  expect_true(identical(
    kaya(pop = 2, gdp = 3, enInt = 4, carbInt = 5, output_type = "C"),
    120 / 3.67)
  )
  expect_true(identical(
    kaya(pop = 0, gdp = 3, enInt = 2, carbInt = 1, output_type = "C"),
    0)
  )
})

context("'kaya' function - correct handling of wrong inputs")

test_that("Correct check that all numeric arguments need to be numeric", {
  params <- c(pop = 2, gdp = 3, enInt = 4, carbInt = 5)
  for (i in 1:4) {
    params_wrong <- params
    params_wrong[i] <- as.character(params_wrong[i])
    expect_error(
      kaya(params_wrong[1], params_wrong[2], params_wrong[3], params_wrong[4]),
      "type 'numeric'"
    )
  }
})

test_that("Correct check that all numeric arguments must be >= 0", {
  params <- c(pop = 2, gdp = 3, enInt = 4, carbInt = 5)
  for (i in 1:4) {
    params_wrong <- params
    params_wrong[i] <- -10
    expect_error(
      kaya(params_wrong[1], params_wrong[2], params_wrong[3], params_wrong[4]),
      "not >= 0"
    )
  }
})

test_that("Correct check that all numeric arguments must be of length 1", {
  params <- list(pop = 2, gdp = 3, enInt = 4, carbInt = 5)
  for (i in 1:4) {
    params_wrong <- params
    params_wrong[[i]] <- c(1:4)
    expect_error(
      kaya(params_wrong[[1]], params_wrong[[2]], params_wrong[[3]], params_wrong[[4]]),
      "Must have length 1"
    )
  }
})



test_that("Correct checks for output_type", {
  expect_error(
    kaya(1, 2, 3, 4, "not_valid_output_type"),
    "must be 'CO2' or 'C'"
  )
})
