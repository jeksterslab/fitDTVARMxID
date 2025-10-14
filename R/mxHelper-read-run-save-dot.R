.MxHelperReadRunSave <- function(input,
                                 tries_explore = 1000,
                                 tries_local = 1000,
                                 max_attempts = 1000,
                                 grad_tol = 1e-2,
                                 hess_tol = 1e-8,
                                 eps = 1e-6,
                                 factor = 10,
                                 quiet = FALSE,
                                 check_hess = TRUE) {
  tryCatch(
    {
      if (check_hess) {
        model <- readRDS(input)
        fit <- .MxHelperEnsureGoodHessian(
          model = model,
          tries_explore = tries_explore,
          tries_local = tries_local,
          max_attempts = max_attempts,
          grad_tol = grad_tol,
          hess_tol = hess_tol,
          eps = eps,
          factor = factor,
          quiet = quiet
        )
        saveRDS(object = fit, file = input)
        readRDS(file = input)
      } else {
        model <- readRDS(input)
        fit <- OpenMx::mxTryHard(
          model = model,
          silent = quiet
        )
        saveRDS(object = fit, file = input)
        readRDS(file = input)
      }
    },
    error = function(e) {
      message("Error while fitting model: ", conditionMessage(e))
      message(paste0("Check ", input))
    },
    warning = function(w) {
      message("Warning while fitting model: ", conditionMessage(w))
      message(paste0("Check ", input))
    }
  )
}
