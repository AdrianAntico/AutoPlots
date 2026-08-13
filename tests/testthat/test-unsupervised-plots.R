test_that("unsupervised plot APIs render package-owned numerical results", {
  embedding <- data.table::data.table(
    component_1 = seq_len(12), component_2 = rev(seq_len(12)),
    cluster = rep(c("A", "B"), 6L)
  )
  plots <- list(
    Plot.ClusterEmbedding(embedding, "component_1", "component_2", "cluster"),
    Plot.ExplainedVariance(c(0.6, 0.3, 0.1)),
    Plot.ClusterSizes(c(A = 6, B = 6)),
    Plot.AnomalyScores(seq(0, 1, length.out = 12), rep(c(FALSE, TRUE), 6L)),
    Plot.NeighborDistances(seq(0.1, 1.2, by = 0.1)),
    Plot.MissingnessRates(c(a = .1, b = .3)),
    Plot.TemporalEvents(seq_len(12), sin(seq_len(12)), c(4L, 9L))
  )
  expect_true(all(vapply(plots, inherits, logical(1L), "htmlwidget")))
})

test_that("unsupervised plot APIs reject malformed numerical contracts", {
  expect_error(Plot.ClusterEmbedding(data.table::data.table(x = 1), "x", "y"),
    "Missing required plot columns")
  expect_error(Plot.ExplainedVariance(c(0.5, NA_real_)), "finite")
  expect_error(Plot.ClusterSizes(c(2, -1)), "non-negative")
  expect_error(Plot.AnomalyScores(1:3, c(TRUE, FALSE)), "align")
  expect_error(Plot.NeighborDistances(c(1, Inf)), "finite")
  expect_error(Plot.MissingnessRates(c(-.1, .2)), "proportions")
  expect_error(Plot.TemporalEvents(1:2, 1:3), "align")
})
