\name{SomaDataIO-package}
\alias{SomaDataIO-package}
\alias{SomaDataIO}
\alias{Soma}
\alias{SomaSciences}
\docType{package}

\title{
SomaData Input/Output Package
}

\description{
This package loads the SomaSciences proprietary data file, called an ADAT file ("*.adat"). It also provides auxiliary functions for extracting and manipulating relevant information from an ADAT, as well as exporting a modified ADAT to file.
}


\details{
\tabular{ll}{
Package:   \tab SomaDataIO \cr
Type:      \tab Package \cr
Version:   \tab 1.5.0 \cr
Date:      \tab 2015-06-05 \cr
License:   \tab GPL (>= 2) \cr
LazyLoad:  \tab yes \cr
LazyData:  \tab yes \cr
}

To load an ADAT file into the global workspace, call \code{read.adat("path/to/file.adat")}. This function parses the main data table into a \code{data.frame} and assigns the remaining data from the "*.adat" file as object \code{attributes}, see (\code{attributes(adat)}). The other functions in the package make extracting data from and manipulating the newly created object convenient.

Those familiar with micro-array data analysis and associated packages (e.g. \code{Biobase}) will notice that the feature data (proteins) are arranged as columns and the samples (arrays) are the rows of the data frame. This is the inverse of typical micro-array data. This conflict can be easily solved using the transpose function (\code{t()}), which is part of the "package:base" in a standard R installation. In addition, those familiar with the standard \code{ExpressionSet} object, available from \code{Bioconductor}, might find the functions \code{\link{adat2eSet}} and \code{\link{melt.eSet}} particularly useful.
}

\author{
Stu Field \cr
Michael R. Mehan \cr
Michael Schaffer (Janssen Research and Development)
}

\examples{
# To load an original adat call read.adat:
# my.adat <- read.adat("path/to/file.adat")
#
# The sample.adat object is pre-defined
# and is available upon calling require(SomaDataIO)
# examples below use "sample.adat"
#
# Parse column names
meta.data <- get.meta(sample.adat)
meta.data
somamers <- get.somamers(sample.adat)
length(somamers)
get.somamers(sample.adat, n=TRUE)
somamer.list <- get.soma.names(sample.adat)
somamer.list$MMP10.3743.1.2
#
# Get SOMAmer data
somamer.table <- get.soma.data(sample.adat)
names(somamer.table)
table(somamer.table$Dilution)
#
# all in one call
# attach.soma.data(sample.adat)     # attaches useful objects directly to .GlobalEnv
#
# Find all MMP analytes and check somamer.table
mmps <- grep("^MMP", somamers, value=TRUE)
mmps
somamer.table[ mmps, ]
}
