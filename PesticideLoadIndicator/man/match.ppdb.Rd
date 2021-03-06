% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_ppdb.R
\name{match.ppdb}
\alias{match.ppdb}
\title{Expend tables with information on ecotoxicity, fate and human health properties from PPDB}
\usage{
match.ppdb(substances, products, folder)
}
\arguments{
\item{substances}{Dataframe describing active ingredients of the applied pesticide products and their CAS number.}

\item{products}{Dataframe with raw pesticide application data.}

\item{folder}{Folder with exported xlsx files from PPDB containing information on active ingredient properties.}
}
\value{
names Lists with updated substance and product data frames.
}
\description{
Expend tables with information on ecotoxicity, fate and human health properties from PPDB
}
