test_that("XY plot and Cat groups works", {
  g1 <- plot_xy_CatGroup(mtcars, 
                        disp, hp,
                        cyl,
                        TextXAngle = 45)
  expect_s3_class(g1, "ggplot")
  expect_equal(g1$theme$text$size, 20)
  expect_equal(g1$labels$x[1], "disp")
  expect_equal(g1$labels$y[1], "hp")
  if (utils::packageVersion("ggplot2") <= "3.4.2") {
    expect_equal(g1$guides$x$angle, 45)
  } else {
    expect_equal(g1$guides$x$angle, 45)
  }
})
