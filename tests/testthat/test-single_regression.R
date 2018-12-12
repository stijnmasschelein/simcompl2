context("demand function specification")

test_that("no nan result", {
  sample = simcompl2::create_sample()
  reg = simcompl2::single_reg(data = sample,
                              formula = x1 ~ x2 + z,
                              label = "Demand",
                              variable = "x2")
  expect_true(!anyNA(reg))
})

context("performance function specification")

test_that("no nan result", {
  sample = simcompl2::create_sample()
  reg = simcompl2::single_reg(data = sample,
              formula = y ~ x1 * x2 + x1 * z + x2 * z + x1^2 + x2^2,
              label = "Performance",
              variable = "x1:x2")
  expect_true(!anyNA(reg))
})

context("bootstrap")

test_that("no nan result", {
  sample = simcompl2::create_sample()
  reg = boot_reg(data = sample,
                 formula = x1 ~ x2 + z,
                 label = "Demand_boot",
                 variable = "x2",
                 bootR = 500)
  expect_true(!anyNA(reg))
})
