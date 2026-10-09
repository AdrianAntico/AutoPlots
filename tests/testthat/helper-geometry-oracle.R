# Independent geometry oracle. This file must not call AutoPlots.

oracle_identity_limits <- function(value) {
  lo <- min(0, value)
  hi <- max(0, value)
  span <- hi - lo
  if (span == 0) return(c(ymin = 0, ymax = 1))
  pad <- 0.05 * span
  c(ymin = lo - if (lo < 0) pad else 0, ymax = hi + if (hi > 0) pad else 0)
}

oracle_log_limits <- function(value) {
  encoded <- log10(value)
  pad <- 0.05 * abs(encoded)
  if (!is.finite(pad) || pad == 0) pad <- 0.05
  c(ymin = encoded - pad, ymax = encoded + pad)
}

# Recomputes device coordinates on a fresh png. `ys` is a list of user-coordinate vectors.
oracle_device <- function(ylims, ys, width = 800, height = 500, res = 96) {
  n <- length(ylims)
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = width, height = height, units = "px", res = res, bg = "white")
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mfrow = c(n, 1L), mar = c(5.2, 4.2, 2.8, 0.8), family = "sans")
  out <- vector("list", n)
  for (i in seq_len(n)) {
    graphics::plot(NA, xlim = c(0.5, 1.5), ylim = ylims[[i]], xlab = "", ylab = "",
      main = "", sub = "", axes = FALSE, xaxs = "i", yaxs = "i")
    out[[i]] <- list(
      x = graphics::grconvertX(c(0.5, 1, 1.5), from = "user", to = "device"),
      y = graphics::grconvertY(ys[[i]], from = "user", to = "device"))
  }
  out
}

oracle_png_signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
