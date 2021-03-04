rm -f PesticideLoadIndicator/PesticideLoadIndicator*.tar.gz

cd PesticideLoadIndicator
RScript -e 'library(devtools); devtools::build_vignettes(); devtools::document();'
R CMD build .
R CMD check --as-cran PesticideLoadIndicator*.tar.gz
