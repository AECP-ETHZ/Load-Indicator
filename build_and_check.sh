rm -r AECPLoadIndicator*.tar.gz

cd AECPLoadIndicator
RScript -e 'library(devtools); devtools::build_vignettes; devtools::document()'
R CMD build .
R CMD check --as-cran AECPLoadIndicator*.tar.gz
