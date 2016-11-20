\name{get.meta}
\alias{get.meta}

\title{
Get Meta Data
}

\description{
Return a character vector of field names of the meta data for an adat data frame, "soma.data" object. 
}

\usage{
get.meta(adat, n=FALSE)
}

\arguments{
  \item{adat}{
A "soma.adat" object created using \code{\link{read.adat}}. Alternatively, a character vector of names corresponding to the \code{names} of the data frame can be passed.
}
  \item{n}{
Logical. Rather then a vector of meta data names, should the number of meta fields in \code{adat} be returned?
}
}

\details{
}

\value{
A character vector of adat meta data names or, an integer number, the length of the meta data names.
}

\references{
%% ~put references to the literature/web site here ~
}

\author{
Stu Field
}

\note{
%%  ~~further notes~~
}

\seealso{
}

\examples{
meta.vec <- get.meta(sample.adat)
head(meta.vec, 20)
get.meta(sample.adat, n=TRUE)
identical(get.meta(sample.adat), get.meta(names(sample.adat))) # TRUE; pass either vector or data frame
}

