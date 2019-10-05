context("kolada")

library(testthat)
result <- readRDS(file = "./data/municipality.rda")

query_big <- "http://api.kolada.se/v2/data/kpi/N00945/year/2009,2007"

test_that("Outputs are incorrect", {
  expect_equal(length(kolada(query_big)), 4911)
  expect_equal(kolada(query_big), result)
})