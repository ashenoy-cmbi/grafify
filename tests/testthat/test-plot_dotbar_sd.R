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
  expect_match(as.character(rlang::quo_get_expr(db1$labels$x)), 
               "Genotype")
  expect_match(as.character(db1$labels$y), 
               "PI")
  expect_match(as.character(rlang::quo_get_expr(db1$labels$fill)), 
               "Genotype")
  #check text angle is passed on
  expect_equal(db1$guides$x$angle, 45)
})

test_that("Check dotbar-sd single colour plots", {
  db2 <- plot_dotbar_sd_sc(data_2w_Tdeath, #plotted with grafify
                        Genotype, 
                        PI,
                        TextXAngle = 45,
                        colour = "#AB0001") +
    facet_wrap("Time")
  db2
  #test key layers and data file
  expect_equal(db2$data, data_2w_Tdeath)
  expect_s3_class(db2, "gg")
  expect_equal(db2$theme$text$size, 20)
  #match aesthetics in labels
  expect_match(as.character(rlang::quo_get_expr(db2$labels$x)), 
               "Genotype")
  expect_match(as.character(db2$labels$y), 
               "PI")
  #check text angle is passed on
  expect_equal(db2$guides$x$angle, 45)
})
