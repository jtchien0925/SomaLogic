\name{get.soma.data}
\alias{get.soma.data}

\title{
Get SOMAmer Data
}

\description{
This function takes the Column Meta data (meta data that appears above the protein measurements in the "*.adat" file) from the intact attributes of an "soma.adat" object and compiles them into a data frame for simple manipulation and indexing. The SOMAmer names of the "soma.adat" object become the row names of the data frame.
}

\usage{
get.soma.data(adat)
}

\arguments{
  \item{adat}{
An adat object with intact attributes (i.e. has not been modified thus stripping original attributes), typically created using \code{\link{read.adat}}.
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
A \code{data.frame} with columns corresponding to the column meta data entries in the "soma.adat" object. This data frame has \code{row.names} corresponding to the SOMAmers in the object, which can be retrieved using \code{\link{get.somamers}}. The SOMAmer data for a subset of SOMAmers can be retrieved by indexing into the rows of this \code{data.frame} (see example).
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
\code{\link{get.somamers}} \cr
\code{\link{get.meta}} \cr
\code{\link[SomaGlobals]{is.intact.attributes}}
}

\examples{
# Get SOMAmer data
somamer.table = get.soma.data(sample.adat)
head(somamer.table)                    # First few rows of the data.frame
table(somamer.table$Dilution)          # Print number of SOMAmers in each dilution.
choose5 <- sample(row.names(somamer.table), 5)  # Get 5 random SOMAmers
somamer.table[ choose5, ]              # Print SOMAmer data for these 5
}

