test_that("Check QQ plots", {
  db1 <- plot_qqline(data_2w_Tdeath, #plotted with grafify
                              PI, Genotype,
                              TextXAngle = 45,
                              ColPal = "muted",
                              ColRev = T) +
    facet_wrap("Time")
  #db1
  ## new 13/06/2025 tests PASS
  labels <- get_labs(db1)
  expect_match(labels$y, "sample")
  expect_match(labels$x, "theoretical")
  #expect_true(inherits(db1$labels, c("ggplot2::labels", "labels")))
  ##
  #test key layers and data file
  expect_equal(db1$data, data_2w_Tdeath)
  expect_s3_class(db1, "gg")
  expect_equal(db1$theme$text$size, 20)
  #match aesthetics in labels
  #expect_match(as.character(db1$labels$y), "sample")
  #check text angle is passed on
  expect_equal(get_guide_angle(db1, "x"), 45)
})


test_that("Check histogram plots", {
  db2 <- plot_histogram(data_2w_Tdeath, #plotted with grafify
                              PI,Genotype,
                              TextXAngle = 45) +
    facet_wrap("Time")
  db2
  #test key layers and data file
  expect_equal(db2$data, data_2w_Tdeath)
  expect_s3_class(db2, "gg")
  expect_equal(db2$theme$text$size, 20)
  #match aesthetics in labels
  #expect_match(as.character(db2$labels$y), "count")
  #check text angle is passed on
  expect_equal(get_guide_angle(db2, "x"), 45)
  ## new 13/06/2025 tests PASS
  labels <- get_labs(db2)
  expect_match(labels$y, "Counts")
  expect_match(labels$x, "PI")
  #expect_true(inherits(db2$labels, c("ggplot2::labels", "labels")))
  ##

})


test_that("Check density plots", {
  db2 <- plot_density(data_2w_Tdeath, #plotted with grafify
                        PI,Genotype,
                        TextXAngle = 45) +
    facet_wrap("Time")
  db2
  #test key layers and data file
  expect_equal(db2$data, data_2w_Tdeath)
  expect_s3_class(db2, "gg")
  expect_equal(db2$theme$text$size, 20)
  #match aesthetics in labels
  #expect_match(as.character(db2$labels$y), "count")
  #check text angle is passed on
  #expect_equal(get_guide_angle(db2, "x"), 45)

  ## new 13/06/2025 tests PASS
  labels <- get_labs(db2)
  #expect_true(inherits(db2$labels, c("ggplot2::labels", "labels")))
  ##

})
