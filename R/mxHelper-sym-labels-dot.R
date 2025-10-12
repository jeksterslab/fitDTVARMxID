.MxHelperSymLabels <- function(p,
                               name,
                               sep) {
  out <- .MxHelperFullLabels(
    m = p,
    n = p,
    name = name,
    sep = sep
  )
  out[upper.tri(out)] <- t(out)[upper.tri(out)]
  out
}
