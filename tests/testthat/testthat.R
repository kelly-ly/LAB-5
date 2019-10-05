context("kolada")

library(testthat)
big_query <- readRDS(file = "../../big_qurey.Rds")
kolada <- kolada_api$new()

# Test the limits of the Kolada API
# For each of query, the result is limited to 5000 items for each request
test_that("test limits of kolada", {
  expect_equal(kolada$search_data(input_kpi="N00945",input_year="2009,2007",input_municipality=""), big_query[1:5000,])
})

# Data query requires at least 2 parameters
test_that("test the limits of api parameters", {
  expect_that(kolada$search_data(input_kpi="N00945",input_municipality="",input_year=""), throws_error())
})

# Test the input and output
test_that("test search_with_title function", {
  result <- kolada$search_with_title(search_type = "municipality", input_str = "lund")
  expect_output(print(result), "count[ \t]*values.id[ \t]*values.title[ \t]*values.type[\r\n]1[ \t]*1[ \t]*1281[ \t]*Lund[ \t]*K")
})

# API return 0 result
test_that("test 0 result found", {
  expect_output(print(kolada$search_with_title(search_type = "municipality", input_str = "no_such_title")), "0 result found.")
  expect_output(print(kolada$search_with_id(search_type = "municipality_groups", input_str = "no_such_id")), "0 result found.")
  expect_output(print(kolada$search_data(input_kpi="N00945",input_municipality="1860",input_year="2022")), "0 result found.")
  expect_output(print(kolada$search_ou("no_such_kpi","V15E144001301,V15E144001101","2009,2008,2007")), "0 result found.")
})

# for search_with_title and search_with_id function, user should choose the search_tyoe from the  selections
test_that("wrong search_type input", {
  expect_output(print(kolada$search_with_title(search_type = "no_such_type", input_str = "lund")), "Wrong Search Type!")
  expect_output(print(kolada$search_with_id(search_type = "no_such_type", input_str = "G124026")), "Wrong Search Type!")
})

#Test the type of return
test_that("test type of return", {
  expect_that(kolada$search_with_title(search_type = "municipality", input_str = "lund"), is_a("data.frame"))
  expect_that(kolada$search_with_id(search_type = "municipality_groups", input_str = "G124026"), is_a("data.frame"))
  expect_that(kolada$search_data(input_kpi="N00945",input_municipality="1860",input_year=""), is_a("data.frame"))
  expect_that(kolada$search_ou("N15033,N15030","V15E144001301,V15E144001101","2009,2008,2007"), is_a("data.frame"))
})

