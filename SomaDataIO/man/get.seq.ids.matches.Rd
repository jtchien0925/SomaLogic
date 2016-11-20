\name{get.seq.ids.matches}
\alias{get.seq.ids.matches}

\title{
Get SeqId Matches
}

\description{
Match two character vectors on the basis of their intersecting SeqIds. This produces a two-column matrix which then can be used as to map between the two sets.
}

\usage{
get.seq.ids.matches(from.list, to.list, show=FALSE)
}

\arguments{
  \item{from.list}{
A character string vector containing SeqIds (e.g. colnames of a "soma.adat" object).
}
  \item{to.list}{
A second character string vector containing SeqIds to match against the first (in \code{from.list}).
}
  \item{show}{
Logical. Should the returned matrix object be returned visibly?
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
An n x 2 matrix [L1 vs L2], where n is the length of the intersect of the matching SeqIds.

\bold{Note:} the final order of the matches/rows is by the input of \emph{L2}, i.e. corresponding to the \code{to.list} (second) argument.

\bold{Important:} by default the matrix object is invisibly returned to avoid dumping excess output to the console (see the \code{show} argument above). To see the object, either call \code{\link{print}} explicitely or assign to a variable and call it.
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
\code{\link[base]{intersect}}
}

\examples{
somamers1 <- setdiff(names(sample.adat), get.meta(sample.adat))
somamers2 <- setdiff(names(sample.adat.hyb.med), get.meta(sample.adat.hyb.med))
#
a <- get.seq.ids.matches(somamers1, somamers2)
b <- get.seq.ids.matches(somamers1[1:100], somamers1[50:500])
head(a); head(b)
#
a <- head(somamers1, 15)
set.seed(99)
b <- sample(head(somamers1, 15))
print(get.seq.ids.matches(a,b))    # sorted by second vector 'b'
}

