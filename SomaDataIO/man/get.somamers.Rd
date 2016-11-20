\name{get.somamers}
\alias{get.somamers}

\title{
Get SOMAmers
}

\description{
Return the SOMAmer names (i.e. the column names for the feature data) from a "soma.adat" data frame object, matrix object, or character vector.
}

\usage{
get.somamers(adat, remove.controls=FALSE, n=FALSE)
}

\arguments{
  \item{adat}{
A "soma.adat" object created using \code{\link{read.adat}}. Alternatively, can be a "matrix" object with column names corresponding to SOMAmer names, or a character vector of SOMAmers.
}
  \item{remove.controls}{
Logical. Should all control and non-human SOMAmers (e.g. "NonHuman", "NonBiotins", "Spuriomers") be removed from the returned value?
}
  \item{n}{
Logical. Return the number of SOMAmers rather than a vector string of SOMAmers.
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
Either a character vector of feature names or an integer number (the length of the feature names).
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
\code{\link[base]{length}} \cr
\code{\link[base]{grep}} \cr
\code{\link[SomaGlobals]{seq.regex}} \cr
}

\examples{
## To load an adat call read.adat (see below). 
## In this example, this call has been replaced with a pre-loaded sample adat
\dontrun{
sample.adat <- read.adat("path/to/file.adat")
}
somamers <- get.somamers(sample.adat)
head(somamers)
get.somamers(sample.adat, TRUE)
get.somamers(sample.adat, n=TRUE)
bb <- get.somamers(names(sample.adat))
identical(somamers,bb)
}

