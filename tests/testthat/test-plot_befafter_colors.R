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
  expect_match(db1$labels$y, 
               "PI")
  #check text angle is passed on
  if (utils::packageVersion("ggplot2") <= "3.4.2") {
    expect_equal(db1$guides$x$angle, 45)
  } else {
    expect_equal(db1$guides$guides$x$params$angle, 45)
  }
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
  expect_match(db2$labels$y, 
               "PI")
  #check text angle is passed on
  if (utils::packageVersion("ggplot2") <= "3.4.2") {
    expect_equal(db2$guides$x$angle, 45)
  } else {
    expect_equal(db2$guides$guides$x$params$angle, 45)
  }
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
  expect_match(as.character(rlang::quo_get_expr(db2$labels$x)), 
               "Genotype")
  expect_match(db2$labels$y, 
               "PI")
  #check text angle is passed on
  
  if (utils::packageVersion("ggplot2") <= "3.4.2") {
    expect_equal(db2$guides$x$angle, 45)
  } else {
    expect_equal(db2$guides$guides$x$params$angle, 45)
  }
})

