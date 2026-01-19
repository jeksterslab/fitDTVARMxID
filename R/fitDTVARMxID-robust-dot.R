.Robust <- function(fit,
                    ncores) {
  threads <- OpenMx::mxOption(
    key = "Number of Threads"
  )
  on.exit(
    OpenMx::mxOption(
      key = "Number of Threads",
      value = threads
    ),
    add = TRUE
  )
  foo <- function(model) {
    utils::capture.output(
      suppressMessages(
        suppressWarnings(
          out <- OpenMx::imxRobustSE(
            model = model,
            details = TRUE
          )
        )
      )
    )
    out
  }
  # nocov start
  if (is.null(ncores)) {
    par <- FALSE
  } else {
    ncores <- min(
      as.integer(ncores),
      parallel::detectCores(),
      length(fit)
    )
    if (ncores > 1) {
      par <- TRUE
    } else {
      par <- FALSE
    }
  }
  # nocov end
  if (par) {
    # nocov start
    OpenMx::mxOption(
      key = "Number of Threads",
      value = 1
    )
    os_type <- Sys.info()["sysname"]
    if (os_type == "Darwin") {
      fork <- TRUE
    } else if (os_type == "Linux") {
      fork <- TRUE
    } else {
      fork <- FALSE
    }
    if (fork) {
      out <- parallel::mclapply(
        X = fit,
        FUN = foo,
        mc.cores = ncores
      )
    } else {
      cl <- parallel::makeCluster(ncores)
      parallel::clusterEvalQ(cl = cl, library(OpenMx))
      on.exit(
        parallel::stopCluster(cl = cl),
        add = TRUE
      )
      out <- parallel::parLapply(
        cl = cl,
        X = fit,
        fun = foo
      )
    }
    # nocov end
  } else {
    out <- lapply(
      X = fit,
      FUN = foo
    )
  }
  out
}
