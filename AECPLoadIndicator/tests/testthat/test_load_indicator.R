test_that("load indicator produces known result", {
  library(readxl)
  substances <- read_excel("./Table_R_substances_example.xlsx")
  products <- read_excel("./Table_R_products_example.xlsx")
  print(compute_pesticide_load_indicator(substances, products))
})
