test_that("make 1w data functions", {
  d1 <- make_1way_data(c(10, 20, 15), 
                       25, 
                       2) #make data
  d2 <- d1 %>% 
    dplyr::group_by(FixFac_1) %>%  
    dplyr::summarise(M = mean(Values))
  expect_equal(d2$M[1], 10, tolerance = 4)
  expect_equal(d2$M[2], 20, tolerance = 4)
  expect_equal(d2$M[3], 15, tolerance = 4)
  m1 <- lm(Values ~ FixFac_1, data = d1)
  expect_equal(summary(m1)$sigma, 2, tolerance = 2)
})

test_that("make 1w RB data functions", {
  d1 <- make_1way_rb_data(c(10, 20, 15),
                          25, 
                          25, 
                          5) #make data
  d2 <- d1 %>% 
    dplyr::group_by(FixFac_1) %>%  
    dplyr::summarise(M = mean(Values))
  expect_equal(d2$M[1], 10, tolerance = 5)
  expect_equal(d2$M[2], 20, tolerance = 5)
  expect_equal(d2$M[3], 15, tolerance = 5)
  m1 <- lmer(Values ~ FixFac_1+(1|RandFac), 
             data = d1)
  expect_equal(summary(m1)$sigma, 5, tolerance = 2)
})
