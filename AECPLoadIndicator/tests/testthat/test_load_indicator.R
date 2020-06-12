test_that("load indicator produces known result", {
  library(readxl)
  substances <- read_excel("./Table_R_substances_example.xlsx")
  products <- read_excel("./Table_R_products_example.xlsx")

  result <- compute_pesticide_load_indicator(substances, products)

  expected_col_names <- c(
    "product", "crop",
    "sum.risk.score", "reference.sum.risk.scores",
    "formula", "amount.applied",
    "standard.doses", "HL",
    "TL", "FL",
    "L", "STI",
    "LI"
  )

  expect_equal(names(result), expected_col_names);
  expect_equal(dim(result), c(4, 13));

  expect_equal(mean(result$HL), 0.348, tolerance=1e-3);
  expect_equal(mean(result$TL), 0.168, tolerance=1e-3);
  expect_equal(mean(result$L), 0.634, tolerance=1e-3);
  expect_equal(mean(result$STI), 0.763, tolerance=1e-3);
  expect_equal(mean(result$LI), 0.442, tolerance=1e-3);

})
