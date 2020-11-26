library(AECPLoadIndicator)


products <- read.table("za_products.csv", sep=";", dec=",", head=TRUE)
substances <- read.table("za_substance_clean3.csv", sep=";", dec=",", head=TRUE)

result <- compute_pesticide_load_indicator(substances, products)
head(result)
