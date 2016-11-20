\name{vec.diff}
\alias{vec.diff}

\title{
Diff Two Vectors
}

\description{
A convenient diff tool to determine where two character vectors differ and how.
}

\usage{
vec.diff(x, y, verbose=getOption("verbose"))
}

\arguments{
  \item{x}{
First vector to compare.
}
  \item{y}{
Second vector to compare.
}
  \item{verbose}{
Should diff summary be printed directly to the console?
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
An invisible list is returned with the set diffs of each vector. The elements of the list are:
\item{first.only }{ Entries unique to the first vector. }
\item{second.only }{ Entries unique to the second vector. }
\item{inter }{ The intersect of the two vectors. }
\item{unique }{ All unique entries of the union of the two vectors. }
}

\references{
%% ~put references to the literature/web site here ~
}

\author{
Stu Field \cr
Michael R. Mehan
}

\note{
%%  ~~further notes~~
}

\seealso{
\code{\link{setdiff}} \cr
\code{\link{union}} \cr
\code{\link{intersect}}
}

\examples{
vec.diff(LETTERS[1:10], LETTERS[5:15], verbose=TRUE) # invisible return; summary printed
diffs <- vec.diff(LETTERS[1:10], LETTERS[5:15])
diffs
}

