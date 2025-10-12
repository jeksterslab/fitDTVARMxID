.MxHelperSymFree <- function(p,
                             val) {
  out <- .MxHelperFullFree(
    m = p,
    n = p,
    val = val
  )
  out[upper.tri(out)] <- t(out)[upper.tri(out)]
  out
}
