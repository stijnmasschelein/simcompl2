#
# context("simulated sample")
#
# test_that("No nans in dataset, all variables included", {
#   practices = 2
#   exo_factors = 3
#   observations = 100
#   sample = create_sample(observations = observations, practices = practices,
#                        exo_factors = exo_factors)
#   expect_true(!anyNA(sample))
#   expect_equal(ncol(sample), practices + exo_factors + 1)
# })

context("simulated data")

test_that("No nans in dataset, all variables included", {
  observations = 100
  sample = simcompl::create_sample(obs = observations)
  expect_true(!anyNA(sample))
  expect_equal(ncol(sample), 7)
})
