context("full simulation")

test_that("no nan result, is data.frame", {
  data_params = list(obs = list(100, 200),
                     rate = list(0.5, .25),
                     b2 = list(c(0.25, 0, 0), c(0, 0, 0)))
  test_params = list(list(formula = x1 ~ x2 + z,
                          variable = "x2",
                          label = "demand-control"),
                     list(formula = x1 ~ x2,
                          variable = "x2",
                          label = "demand-no-control",
                          nearly_correction = TRUE))
  sim_params = list(nsim = 2, mc_cores = 1, boot = FALSE)
  sim = simcompl2::run_simulation(data_params = data_params,
                                  test_params = test_params,
                                  sim_params = sim_params)
  expect_true(!anyNA(sim))
  expect_true(class(sim) == "data.frame")
  data_params = list(b2 = list(c(0.25, 0, 0), c(0, 0, 0)),
                     obs = 100)
  test_params = list(list(formula = x1 ~ x2 + z,
                          variable = "x2",
                          label = "demand-control"),
                     list(formula = x1 ~ x2,
                          variable = "x2",
                          label = "demand-no-control"))
  sim_params = list(nsim = 2, mc_cores = 1, boot = TRUE, Rboot = 100)
  sim_boot = simcompl2::run_simulation(data_params = data_params,
                                       test_params = test_params,
                                       sim_params = sim_params)
  expect_true(!anyNA(sim_boot))
  expect_true(class(sim_boot) == "data.frame")
})
