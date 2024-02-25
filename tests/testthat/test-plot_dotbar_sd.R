test_that("Check dotbar-sd plots", {
  db1 <- plot_dotbar_sd(data_2w_Tdeath, #plotted with grafify
                     Genotype, 
                     PI,
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
  #expect_match(as.character(rlang::quo_get_expr(db1$labels$x)), 
  #             "Genotype")
  #expect_match(db1$labels$y, 
  #             "PI")
  #expect_match(as.character(rlang::quo_get_expr(db1$labels$fill)), 
  #             "Genotype")
  ##check text angle is passed on
  #expect_equal(get_guide_angle(db1, "x"), 45)
})
