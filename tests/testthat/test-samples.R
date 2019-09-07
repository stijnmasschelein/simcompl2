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

test_that("Test some of the contingency parameters", {
  observations = 10000
  params = list(obs = observations, g1 = c(.5, 1, 2),
                h1 = c(-0.5, .5, -1), sd = 1)
  sample = do.call(simcompl2::create_sample, params)
  x_exp = cbind(model.matrix(
    ~ z : x1 + z : x2 + z : x3 + x1:x2 + x1:x3 + x2:x3,
    data = sample))
  yhat = sample$y - x_exp %*% c(0, params$g1, params$h1)
  pval = pnorm(mean(yhat), 0, params$sd / sqrt(observations))
  expect_true(any(pval > 0.001, pval < 0.999))
})
