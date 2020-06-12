rm -r AECPLoadIndicator*.tar.gz

RScript -e 'library(devtools); devtools::document("AECPLoadIndicator")'
R CMD build AECPLoadIndicator
R CMD check --as-cran AECPLoadIndicator*.tar.gz
