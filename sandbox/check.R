library(AECPLoadIndicator)
library(stringr)
library(readxl)


read.excel <- function(excel_file) {
    table <- as.data.frame(read_excel(excel_file))
    names(table) <- make.names(names(table))
    table
}



options(width=200)

s <- read.csv("substances_example.csv");
p <- read.excel("Table_R_products_example.xlsx");
p["Year"] <- 2012;

compute_pesticide_load_indicator_ppdb(s, p, ".")
