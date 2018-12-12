context("simulated data")

test_that("No nans in dataset, all variables included", {
  observations = 100
  sample = simcompl2::create_sample(obs = observations)
  expect_true(!anyNA(sample))
  expect_equal(ncol(sample), 7)
})
