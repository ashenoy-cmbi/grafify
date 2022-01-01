test_that("Check 3d scatter box", {
  sb1 <- plot_3d_scatterbox(data_2w_Tdeath, #plotted with grafify
                            Genotype, 
                            PI, 
                            Time,
                            TextXAngle = 45,
                            ColPal = "muted",
                            ColRev = T)       
  
  sb2 <- ggplot(data = data_2w_Tdeath,  #with ggplot2
                aes(x = Genotype,
                    y = PI,
                    group = interaction(Genotype, Time)))+
    stat_summary(geom = "bar",
                 width = .7,
                 colour = "black",
                 fun = "mean",
                 size = 1,
                 aes(fill = Time),
                 position = position_dodge(width = 0.8))+
    geom_point(size = 2.5, aes(shape = Time),
               stroke = 1,
               colour = "black",
               position = position_jitterdodge(dodge.width = 0.8,
                                               jitter.width = .1))+
    stat_summary(geom = "errorbar",
                 width = 0.2,
                 fun.data = "mean_sdl",
                 size = 1,
                 fun.args = list(mult = 1),
                 position = position_dodge(width = 0.8))+
    scale_shape_manual(values = 0:25)+
    labs(x = "Genotype", 
         y = "PI", 
         shape = "Time")+
    theme_classic(base_size = 20)+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = 0))+
    scale_fill_grafify(palette = "all_grafify",
                       reverse = FALSE)
  
  #test key layers and data file
  expect_equal(sb1$data, sb2$data)
  expect_s3_class(sb1, "ggplot")
  expect_equal(sb1$theme$text$size, 20)
  #match aesthetics in labels
  expect_match(as.character(rlang::quo_get_expr(sb1$labels$x)), 
               "Genotype")
  expect_match(as.character(sb1$labels$y), 
               "PI")
  expect_match(as.character(rlang::quo_get_expr(sb1$labels$shape)), 
               "Time")
  expect_match(as.character(rlang::quo_get_expr(sb1$labels$fill)), 
               "Time")
  #check text angle is passed on
  expect_equal(sb1$guides$x$angle, 45)
})
