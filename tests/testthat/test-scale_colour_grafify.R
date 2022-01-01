test_that("Check colour and fill scales", {
  db1 <- ggplot(data = data_doubling_time,
                aes(x = Student,
                    y = Doubling_time)) +
    geom_boxplot(aes(fill = Student),
                 alpha = 0.5)+
    geom_point(aes(colour = Student),
               size = 3)+
    scale_fill_grafify(palette = "muted")+
    scale_colour_grafify(palette = "bright")
  db1
  #test key layers and data file
  expect_equal(db1$data, data_doubling_time)
  expect_s3_class(db1, "gg")
  #match aesthetics in labels
  expect_match(db1$labels$x,"Student")
  expect_match(db1$labels$y, "Doubling_time")
  expect_match(db1$labels$fill, "Student")
  expect_match(db1$labels$colour, "Student")
  expect_match(db1$scales$scales[[1]]$scale_name, "muted")
  expect_match(db1$scales$scales[[2]]$scale_name, "bright")
})

test_that("Check color and fill _2 scales", {
  db1 <- ggplot(data = data_doubling_time,
                aes(x = Student,
                    y = Doubling_time)) +
    geom_boxplot(aes(fill = Student),
                 alpha = 0.5)+
    geom_point(aes(colour = Student),
               size = 3)+
    scale_fill_grafify2(palette = "muted")+
    scale_color_grafify2(palette = "bright")
  db1
  #test key layers and data file
  expect_equal(db1$data, data_doubling_time)
  expect_s3_class(db1, "gg")
  #match aesthetics in labels
  expect_match(db1$labels$x,"Student")
  expect_match(db1$labels$y, "Doubling_time")
  expect_match(db1$labels$fill, "Student")
  expect_match(db1$labels$colour, "Student")
  expect_match(db1$scales$scales[[1]]$scale_name, "muted")
  expect_match(db1$scales$scales[[2]]$scale_name, "bright")
})


test_that("Check colour and fill _2 scales", {
  db1 <- ggplot(data = data_doubling_time,
                aes(x = Student,
                    y = Doubling_time)) +
    geom_boxplot(aes(fill = Student),
                 alpha = 0.5)+
    geom_point(aes(colour = Student),
               size = 3)+
    scale_fill_grafify2(palette = "muted")+
    scale_colour_grafify2(palette = "bright")
  db1
  #test key layers and data file
  expect_equal(db1$data, data_doubling_time)
  expect_s3_class(db1, "gg")
  #match aesthetics in labels
  expect_match(db1$labels$x,"Student")
  expect_match(db1$labels$y, "Doubling_time")
  expect_match(db1$labels$fill, "Student")
  expect_match(db1$labels$colour, "Student")
  expect_match(db1$scales$scales[[1]]$scale_name, "muted")
  expect_match(db1$scales$scales[[2]]$scale_name, "bright")
})

test_that("Check color and fill _2 scales", {
  db1 <- ggplot(data = data_doubling_time,
                aes(x = Student,
                    y = Doubling_time)) +
    geom_boxplot(aes(fill = Student),
                 alpha = 0.5)+
    geom_point(aes(colour = Student),
               size = 3)+
    scale_fill_grafify2(palette = "muted")+
    scale_color_grafify2(palette = "bright")
  db1
  #test key layers and data file
  expect_equal(db1$data, data_doubling_time)
  expect_s3_class(db1, "gg")
  #match aesthetics in labels
  expect_match(db1$labels$x,"Student")
  expect_match(db1$labels$y, "Doubling_time")
  expect_match(db1$labels$fill, "Student")
  expect_match(db1$labels$colour, "Student")
  expect_match(db1$scales$scales[[1]]$scale_name, "muted")
  expect_match(db1$scales$scales[[2]]$scale_name, "bright")
})


test_that("Check continuous scale", {
  db1 <- ggplot(data = data_2w_Tdeath,
                aes(x = Genotype,
                    y = PI)) +
    geom_point(aes(colour = Time2),
               size = 3)+
    scale_colour_grafify_c()
  db1
  #test key layers and data file
  expect_equal(db1$data, data_2w_Tdeath)
  expect_s3_class(db1, "gg")
  #match aesthetics in labels
  expect_match(db1$labels$x,"Genotype")
  expect_match(db1$labels$y, "PI")
  expect_match(db1$labels$colour, "Time2")
  expect_match(db1$scales$scales[[1]]$scale_name, "gradientn")
})
