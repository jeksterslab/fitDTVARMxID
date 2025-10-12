.MxHelperDiagFree <- function(p,
                              val) {
  diag(
    .MxHelperFullFree(
      m = p,
      n = p,
      val = val
    )
  )
}
