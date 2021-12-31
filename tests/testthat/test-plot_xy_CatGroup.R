test_that("XY plot and groups works", {
  g1 <- plot_xy_CatGroup(mtcars, 
                        disp, hp,
                        cyl,
                        TextXAngle = 45)
  expect_s3_class(g1, "ggplot")
  expect_equal(g1$theme$text$size, 20)
  expect_equal(g1$labels$x[1], "disp")
  expect_equal(g1$labels$y[1], "hp")
  expect_equal(g1$guides$x$angle, 45)
})

test_that("XY plot and groups works", {
  g2 <- plot_xy_NumGroup(mtcars, 
                        disp, hp,
                        cyl,
                        symsize = 5,
                        symthick = .5)
  expect_s3_class(g2, "ggplot")
  expect_equal(g2$labels$x[1], "disp")
  expect_equal(g2$labels$y[1], "hp")
  expect_equal(g2$layers[[1]]$aes_params$size, 5)
  expect_equal(g2$layers[[1]]$aes_params$stroke, 0.5)
})
