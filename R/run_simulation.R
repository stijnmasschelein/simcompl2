#' Perform a simulation
#' @export
#' @param data_params List of parameters to be used to simulate data.
#' @param test_params List of with formula, variable, and label to use as a
#' test for the complementarity
#' @param sim_params List of simulation specific parameters.
#' \describe{
#' \item{\code{nsim}}{Integer for the number of replications in the
#' simulation}
#' \item{\code{mc_cores}}{Integer for the number of cores to be used in the
#'  parallel}
#' \item{\code{boot}}{Boolean whether the bootstrap version is used
#' to estimate standard errors.}
#' \item{\code{bootR}}{The number of bootstrap replications}
#' }

run_simulation <- function(data_params, test_params,
                           sim_params = list(nsim = 100,
                                             mc_cores = 1,
                                             boot = FALSE,
                                             bootR = 2000)){
  data_params_grid <- expand.grid(data_params)
  result <- list()
  n <- nrow(data_params_grid)
  for (i in 1:n){
    result[[i]] <- run_1param_simulation(
      data_params = data_params_grid[i,],
      test_params = test_params,
      sim_params = sim_params)
  }
  df <- do.call(rbind, result)
  rownames(df) <- NULL
  return(df)
}

run_1param_simulation <- function(data_params, test_params, sim_params){
  result <- parallel::mclapply(1:sim_params$nsim,
                               run_1_simulation,
                               data_params = data_params,
                               test_params = test_params,
                               sim_params = sim_params,
                               mc.cores = sim_params$mc_cores)
  df <- do.call(rbind, result)
}

run_1_simulation <- function(x, data_params, test_params, sim_params){
  df <- data_params
  data_params <- lapply(data_params, unlist)
  sample <- do.call(simcompl2::create_sample, data_params)
  result <- lapply(test_params, run_1_test,
                  data = sample, sim_params)
  n <- sum(sapply(result, nrow))
  df <- cbind(df[rep(1, n), ], do.call(rbind, result))
  df$id <- x
  return(df)
}

run_1_test <- function(test_params, data, sim_params){
  params = test_params
  params$data = data
  if (!sim_params$boot){
    result <- do.call(simcompl2::single_reg, params)
  } else {
    params$bootR = sim_params$bootR
    result <- do.call(simcompl2::boot_reg, params)
  }
}
