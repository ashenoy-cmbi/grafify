test_that("make 2w data functions", {
  d1 <- make_2way_data(c(10, 20),
                       c(30, 40),
                       50, 
                       5) #make data
  d2 <- d1 %>% 
    dplyr::group_by(FixFac_1, 
             FixFac_2) %>%  
    dplyr::summarise(M = mean(Values))
  expect_equal(d2$M[1], 10, tolerance = 2)
  expect_equal(d2$M[2], 20, tolerance = 2)
  expect_equal(d2$M[3], 30, tolerance = 2)
  expect_equal(d2$M[4], 40, tolerance = 2)
  m1 <- lm(Values ~ FixFac_1*FixFac_2, data = d1)
  expect_equal(summary(m1)$sigma, 5, tolerance = 2)
})

test_that("make 2w rb data functions", {
  d1 <- make_2way_rb_data(c(10, 20),
                       c(30, 40),
                       50, 
                       25,
                       5) #make data
  d2 <- d1 %>% 
    dplyr::group_by(FixFac_1, 
             FixFac_2) %>%  
    dplyr::summarise(M = mean(Values))
  expect_equal(d2$M[1], 10, tolerance = 2)
  expect_equal(d2$M[2], 20, tolerance = 2)
  expect_equal(d2$M[3], 30, tolerance = 2)
  expect_equal(d2$M[4], 40, tolerance = 2)
  m1 <- lmer(Values ~ FixFac_1*FixFac_2+(1|RandFac),
             data = d1)
  expect_equal(summary(m1)$sigma, 5, tolerance = 2)
})
