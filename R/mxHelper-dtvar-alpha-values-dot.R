.MxHelperDTVARAlphaValues <- function(p,
                                      val) {
  if (is.null(val)) {
    val <- matrix(
      data = 0,
      nrow = p,
      ncol = 1
    )
  } else {
    if (is.vector(val)) {
      val <- matrix(
        data = val,
        nrow = p,
        ncol = 1
      )
    }
    stopifnot(
      is.matrix(val),
      dim(val) == c(p, 1)
    )
  }
  val
}
