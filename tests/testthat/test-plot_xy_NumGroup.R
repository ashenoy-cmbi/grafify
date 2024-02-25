test_that("XY plot and Num groups works", {
  g2 <- plot_xy_NumGroup(mtcars, 
                         disp, hp,
                         cyl,
                         symsize = 5,
                         symthick = .5)
  expect_s3_class(g2, "ggplot")
  #expect_equal(g2$labels$x[1], "disp")
  #expect_equal(g2$labels$y[1], "hp")
  #expect_equal(g2$layers[[1]]$aes_params$size, 5)
  #expect_equal(g2$layers[[1]]$aes_params$stroke, 0.5)
})
