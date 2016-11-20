\name{is.intact.attributes}
\alias{is.intact.attributes}

\title{
Are Attributes Intact?
}

\description{
Uses a series of checks to determine if a "soma.adat" object has a complete set of attributes. If not, this indicates that the object has been modified since the initial \code{\link[SomaData]{read.adat}} call. Modification will strip old attributes, creating a new set of attributes corresponding the current "new" data frame.
}

\usage{
is.intact.attributes(adat, verbose=getOption("verbose"))
}

\arguments{
  \item{adat}{
A "soma.adat" object to query.
}
  \item{verbose}{
Logical. Flag to return where the attribute tests fail.
}
}

\details{
Checks for the presence of both "Header.Meta" and "Col.Meta" in the attribute names. These entries are added during the \code{\link[SomaData]{read.adat}} call. In addition, whithin these sections it also checks for the presence of the following entries:

\bold{"Header.Meta" section}: "HEADER", "COL_DATA", and "ROW_DATA" \cr
\bold{"Col.Meta" section}: "Target", "Units", and "Dilution" \cr

If any of the above they are altered or missing, \code{FALSE} is returned.
}

\value{
\code{TRUE} if attributes are intact, \code{FALSE} otherwise.
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
\code{\link[base]{is.na}} \cr
\code{\link[base]{attributes}} \cr
\code{\link[base]{is.null}}
}

\examples{
\dontrun{
myadat <- read.adat("path/to/file.adat")  # load new ADAT into R
is.intact.attributes(myadat)        # TRUE
myadat2 <- myadata[ 1:10,1:10 ]     # subset strips attributes
is.intact.attributes(myadat2)       # FALSE
}
}

