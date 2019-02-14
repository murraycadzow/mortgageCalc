context("test-amortization_functions")

t_principle <- 250000
t_annual_interest <- 5/100
t_years <- 30

test_that("monthly payment works", {
  expect_equivalent(
    round(
      monthly_payment(principle = t_principle,
                      annual_interest = t_annual_interest,
                      years = t_years),2),
    1342.05)
  expect_equal(2 * 2, 4)
})
