\name{attach.soma.data}
\alias{attach.soma.data}

\title{
Attach SOMAmer Data
}

\description{
Attach SOMAmer data and create pre-defined objects (dictionaries/lookup tables) in the ".GlobalEnv".
}

\usage{
attach.soma.data(adat)
}

\arguments{
  \item{adat}{
adat object from which to create dictionary objects. Must have intact attributes from \code{is.intact.attributes(adat)}.
}
}

\details{
}

\value{
\item{somamers}{Character vector of all SOMAmers in the adat.}
\item{somamer.list}{Convenient lookup table of SOMAmer names to SOMAmer name mapping (i.e. adat columns).}
\item{somamer.table}{Convenient dictionary of SOMAmer data, with rownames for indexing.}
\item{target.names}{Convenient dictionary of "SOMAmer" -> "TargetName" mapping.}
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
\code{\link{get.soma.data}} \cr
\code{\link{get.somamers}} \cr
\code{\link{get.soma.names}}
}

\examples{
attach.soma.data(sample.adat)
head(somamers)
somamer.list[1:6]
head(somamer.table)
}

