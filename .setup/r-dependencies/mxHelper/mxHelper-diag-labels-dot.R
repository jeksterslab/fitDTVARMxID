.MxHelperDiagLabels <- function(p,
                                name,
                                sep) {
  diag(
    .MxHelperFullLabels(
      m = p,
      n = p,
      name = name,
      sep = sep
    )
  )
}
