test_that("Check scatter box plots", {
  sb1 <- plot_scatterbox(data_2w_Tdeath, #plotted with grafify
                            Genotype, 
                            PI,
                            TextXAngle = 45,
                            ColPal = "muted",
                            ColRev = T) +
    facet_wrap("Time")
  
  #test key layers and data file
  expect_equal(sb1$data, data_2w_Tdeath)
  expect_s3_class(sb1, "ggplot")
  expect_equal(sb1$theme$text$size, 20)
  #match aesthetics in labels
  expect_match(as.character(rlang::quo_get_expr(sb1$labels$x)), 
               "Genotype")
  expect_match(sb1$labels$y, 
               "PI")
  expect_match(as.character(rlang::quo_get_expr(sb1$labels$fill)), 
               "Genotype")
  #check text angle is passed on
  expect_equal(get_guide_angle(sb1, "x"), 45)
})

