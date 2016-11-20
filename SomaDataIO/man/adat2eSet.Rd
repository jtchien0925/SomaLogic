\name{adat2eSet}
\alias{adat2eSet}

\title{
Convert ADAT to ExpressionSet
}

\description{
Utility function that converts a SomaLogic "soma.adat" object to an "ExpressionSet" object via the \code{Biobase} package (download from \code{"Bioconductor"}).
}

\usage{
adat2eSet(adat)
}

\arguments{
  \item{adat}{
Adat object as read into the R environment using \code{\link{read.adat}}.
}
}

\details{
This function requires the \code{Biobase} package and must be installed from \code{Bioconductor}. To obtain \code{Biobase}, type the following at the R console:

source("http://bioconductor.org/biocLite.R") \cr
biocLite("Biobase")
}

\value{
An object of class \code{ExpressionSet}.
}

\references{
http://www.bioconductor.org/packages/release/bioc/html/Biobase.html
}

\author{
Stu Field
}

\note{
}

\seealso{
\code{Biobase}
}

\examples{
require(Biobase)
ExSet <- adat2eSet(sample.adat)
class(ExSet)
ExSet
}
