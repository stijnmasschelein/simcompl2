context("simulated data")

test_that("No nans in dataset, all variables included", {
  observations = 100
  sample = simcompl2::create_sample(obs = observations)
  expect_true(!anyNA(sample))
  expect_equal(ncol(sample), 7)
  sample_discrete = simcompl2::create_sample(obs = observations,
                                             xinteger = c(TRUE,
                                                          TRUE,
                                                          FALSE))
  expect_true(!anyNA(sample))
  expect_equal(ncol(sample), 7)
  expect_true(any(sample_discrete$x1 %in% c(1, -1)))
  expect_true(any(sample_discrete$x2 %in% c(1, -1)))
  expect_false(any(sample_discrete$x3 %in% c(1, -1)))
})
