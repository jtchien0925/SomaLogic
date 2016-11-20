\name{get.soma.names}
\alias{get.soma.names}

\title{
Get SOMAmer Names
}

\description{
Creates a lookup table (or dictionary) as a named list of SOMAmer names. SOMAmer names are a concatenation of "EntrezGeneSymbol" + "SeqId" and are themselves as the names of the list object. This is a convenient tool used to quickly access or index into a data frame with a unique SOMAmer name, without knowing the complete SeqId string using the "auto-complete" functionality of R list objects with a "names" attribute.
}

\usage{
get.soma.names(adat)
}

\arguments{
  \item{adat}{
A "soma.adat" object containing SOMAmer names as its names. See \code{\link{get.somamers}}.
}
}

\details{
	This function returns a named list. This list allows the user to access the name of a SOMAmer at the command line using auto-completion. For example, if the result of this function is saved as \code{somamer.list}, then typing \code{somamer.list$IL} followed by 2x tab-key will list all proteins that begin with "IL". See the example for more details.
}

\value{
A named list containing all the SOMAmer names. The key-value pairs are identical in this structure which provides auto-completion of SOMAmer names at the command line.
}

\references{
%% ~put references to the literature/web site here ~
}

\author{
Stu Field
}

\seealso{
\code{\link{attach.soma.data}} \cr
\code{\link{get.somamers}}
}

\examples{
# To load an adat call read.adat(). 
# In this example, this call has been replaced with a pre-loaded adat. 
\dontrun{
sample.adat <- read.adat("path/to/file.adat")
}
somamer.list <- get.soma.names(sample.adat)
head(somamer.list)
somamer.list$MMP10.3743.1.2
# This example shows how you might plot 2 particular SOMAmers from the
# command line using the object list called 'somamer.list'.
par(mfrow=c(1,2))
sapply(c(somamer.list$IL10.2773.50.2, somamer.list$SERPINA3.2879.9.2), function(soma) {
	plot.data <- sample.adat[,soma]
	plot(plot.data, main=soma, xlab="Samples", ylab="RFU", col=rainbow(length(plot.data)))})
}

