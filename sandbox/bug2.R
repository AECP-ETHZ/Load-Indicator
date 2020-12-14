library(PesticideLoadIndicator)

products <- read.table("za_products_empty.csv", sep=";", dec=",", head=TRUE)
substances <- read.table("za_substance_empty.csv", sep=";", dec=",", head=TRUE, fileEncoding="latin1")

colnames(substances)[4] <- "CAS.number"
head(substances)
head(products)
products$Year <- 2012

x<-match.ppdb(substances, products, ".");
#print(colnames(substances))
#print(colnames(products))

