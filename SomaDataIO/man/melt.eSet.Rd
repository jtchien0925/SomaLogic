\name{melt.eSet}
\alias{melt.eSet}

\title{
Convert (melt) to Long Format
}

\description{
Unility function that converts an \code{"ExpressionSet"} object from the "wide" data format to the "long" format (i.e. melt).
}

\usage{
melt.eSet(eSet)
}

\arguments{
  \item{eSet}{
An \code{"ExpressionSet"} object, typically created using \code{\link{adat2eSet}}.
}
}

\details{
This function requires the \code{reshape} package. If not installed, please install via:

install.packages("reshape")
}

\value{
An object of class "data.frame" consisting of the long format conversion of an \code{"ExpressionSet"} object.
}

\references{
%% ~put references to the literature/web site here ~
}

\author{
Stu Field
}

\note{
}

\seealso{
\code{\link{adat2eSet}} \cr
\code{melt} (pkg:reshape) \cr
\code{Biobase}
}

\examples{
require(Biobase)
ex.set <- adat2eSet(sample.adat)  # convert to ExpressionSet object; requires Biobase
require(reshape)
adat.long <- melt.eSet(ex.set)    # convert ExpressionSet object to long format
}

