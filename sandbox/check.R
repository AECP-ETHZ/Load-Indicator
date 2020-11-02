library(AECPLoadIndicator)
library(stringr)
library(readxl)


read.excel <- function(excel_file) {
    table <- as.data.frame(read_excel(excel_file))
    names(table) <- make.names(names(table))
    table
}



options(width=200)

#s <- read.csv("substances_example.csv");
#p <- read.excel("Table_R_products_example.xlsx");
#p["Year"] <- 2012;

#4  000467-69-6 Flurenolcarbonsäure  Aniten        0.0650               12.5              20          5100             2.5                354                  2.5                    10
#5     467-69-6            Flurenol  Aniten        0.0657               12.5              20          5100             2.5                354                  2.5                    10
#6    1918-00-9             Dicamba  Aniten        0.0438               12.5              20          5100             2.5                354                  2.5                    10
#7      94-74-6                MCPA  Aniten        0.2190

# print defaults
print(default.load.factors);

# arguments are:
# folder, product, year, formula, substances, cas-numbers, concentrations, ...
compute_pesticide_load_indicator_single(".", "Aniten", 2012, 1.5,
                                        c('Flurenolcarbonsäure', 'Flurenol', 'Dicamba', 'MCPA'),
                                        c('000467-69-6', '467-69-6', '1918-00-0', '94-74-6'),
                                        c(0.065, 0.0657, 0.0438, 0.2190),
                                        # overrun defaults:
                                        Load.Factor.SCI=30,
                                        Load.Factor.Mammals=3
                                        )
