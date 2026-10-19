testthat::test_that("toolbox geometry uses ECharts' public option names", {
  p <- echarts4r::e_charts(data.table::data.table(x=1:3, y=c(2,4,3)), x)
  p <- e_toolbox_full(p, toolbox.orient="vertical", toolbox.heigth=210)
  testthat::expect_identical(p$x$opts$toolbox$orient, "vertical")
  testthat::expect_equal(p$x$opts$toolbox$height, 210)
  testthat::expect_null(p$x$opts$toolbox$toolbox)
})

testthat::test_that("named controls preserve the complete numerical widget", {
  p <- echarts4r::e_charts(data.table::data.table(x=1:3, y=c(2,4,3)), x)
  p <- echarts4r::e_line(p, y)
  p <- e_toolbox_full(p)
  q <- e_toolbox_panel(p)
  testthat::expect_identical(q$x, p$x)
  testthat::expect_length(q$jsHooks$render, 1L)
  testthat::expect_error(e_toolbox_panel(list()), "echarts4r")
})
