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


context("nearly exact")

test_that("no nan result", {
  sample = simcompl2::create_sample()
  reg = simcompl2::single_reg(data = sample,
              formula = y ~ x1 * x2 + x1 * z + x2 * z + x1^2 + x2^2,
              label = "Performance",
              variable = "x1:x2",
              nearly_correction = TRUE)
  expect_true(!anyNA(reg))
  expect_true(reg[1, "df"] > reg[2, "df"])
  expect_true(nrow(reg) > 0)
  reg = simcompl2::single_reg(data = sample,
              formula = x1 ~ x2 + z,
              label = "Performance",
              variable = "x2",
              nearly_correction = TRUE)
  expect_true(!anyNA(reg))
  expect_true(reg[1, "df"] > reg[2, "df"])
  expect_true(nrow(reg) > 0)
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
