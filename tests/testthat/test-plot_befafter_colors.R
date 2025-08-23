test_that("Check before-after colors plots", {
  db1 <- plot_befafter_colors(data_2w_Tdeath, #plotted with grafify
                              Genotype,
                              PI, Experiment,
                              TextXAngle = 45,
                              ColPal = "muted",
                              ColRev = T) +
    facet_wrap("Time")
  db1
  #test key layers and data file
  expect_equal(db1$data, data_2w_Tdeath)
  expect_s3_class(db1, "gg")
  expect_equal(db1$theme$text$size, 20)
  #match aesthetics in labels
  expect_match(as.character(rlang::quo_get_expr(db1$labels$x)),
               "Genotype")
  #expect_match(db1$labels$y,
  #             "PI")
  #check text angle is passed on
  expect_equal(get_guide_angle(db1, "x"), 45)
})

test_that("Check before-after colour plots", {
  db2 <- plot_befafter_colours(data_2w_Tdeath, #plotted with grafify
                               Genotype,
                               PI,Experiment,
                               TextXAngle = 45) +
    facet_wrap("Time")
  db2
  #test key layers and data file
  expect_equal(db2$data, data_2w_Tdeath)
  expect_s3_class(db2, "gg")
  expect_equal(db2$theme$text$size, 20)
  #match aesthetics in labels
  expect_match(as.character(rlang::quo_get_expr(db2$labels$x)),
               "Genotype")
  #expect_match(db2$labels$y,
  #             "PI")
  #check text angle is passed on
  expect_equal(get_guide_angle(db2, "x"), 45)
})

test_that("Check before-after shapes plots", {
  db2 <- plot_befafter_shapes(data_2w_Tdeath, #plotted with grafify
                              Genotype,
                              PI,Experiment,
                              TextXAngle = 45) +
    facet_wrap("Time")
  db2
  #test key layers and data file
  expect_equal(db2$data, data_2w_Tdeath)
  expect_s3_class(db2, "gg")
  expect_equal(db2$theme$text$size, 20)
  #match aesthetics in labels
  #expect_match(as.character(rlang::quo_get_expr(db2$labels$x)),
  #             "Genotype")
  #expect_match(db2$labels$y,
  #             "PI")
  ##check text angle is passed on
  #expect_equal(get_guide_angle(db2, "x"), 45)

  ## new 13/06/2025 tests PASS
  labels <- get_labs(db2)
  expect_match(labels$y, "PI")
  #expect_true(inherits(db2$labels, c("ggplot2::labels", "labels")))
  ##
})

