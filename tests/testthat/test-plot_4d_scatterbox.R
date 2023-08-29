test_that("Check 4d scatter box", {
  sb1 <- plot_4d_scatterbox(data_2w_Tdeath, #plotted with grafify
                            Genotype, 
                            PI, 
                            Time,
                            Experiment,
                            TextXAngle = 45,
                            ColPal = "muted",
                            ColRev = T)       
  
  #test key layers and data file
  expect_equal(sb1$data, data_2w_Tdeath)
  expect_s3_class(sb1, "ggplot")
  expect_equal(sb1$theme$text$size, 20)
  #match aesthetics in labels
  #expect_match(sb1$labels$x, 
  #             "Genotype")
  expect_match(sb1$labels$y, 
               "PI")
  expect_match(as.character(rlang::quo_get_expr(sb1$labels$shape)), 
               "Experiment")
  #expect_match(sb1$labels$fill, 
  #             "Time")
  #check text angle is passed on
  if (utils::packageVersion("ggplot2") <= "3.4.2") {
    expect_equal(sb1$guides$x$angle, 45)
  } else {
    expect_equal(sb1$guides$x$angle, 45)
  }
})
