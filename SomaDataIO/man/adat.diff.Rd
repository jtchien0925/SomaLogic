\name{adat.diff}
\alias{adat.diff}

\title{
Diff Two ADAT Objects
}

\description{
Diff tool for the differences between two adat objects.
}

\usage{
adat.diff(adat1, adat2, tolerance=1e-08)
}

\arguments{
  \item{adat1}{
First "soma.data" adat.
}
  \item{adat2}{
Second "soma.data" adat.
}
  \item{tolerance}{
Tolerance for the difference between numeric vectors (i.e. SOMAmer/feature data). Passed eventually to \code{\link[base]{all.equal}} via an internal function.
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
If there are diffs in the data table, a list of 1) meta data fields with differing values, 2) feature data fields with differing values. Otherwise, the string "Data table is identical" is returned. Results of the individual diagnostic diffs are printed to the console as the function proceeds.
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
\code{\link[base]{all.equal}}
}

\examples{
adat.diff(sample.adat, sample.adat)
adat.diff(sample.adat, sample.adat.hyb.med.cal)
adat.diff(sample.adat, sample.adat.hyb)
adat.diff(sample.adat.hyb, sample.adat.hyb.med)
}

