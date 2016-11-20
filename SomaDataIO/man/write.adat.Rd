\name{write.adat}
\alias{write.adat}

\title{
Write (Export) SomaLogic ADATs
}

\description{
Write an exisitng ADAT (\code{"soma.adat"} R object) to file. Either the adat object itself or its parent must have intact attributes, i.e. \code{\link{is.intact.attributes}=TRUE}.
}

\usage{
write.adat(dat, file, parent=NULL)
}

\arguments{
  \item{dat}{a data frame containg SOMAmer data to be written.}
  \item{file}{A character string describing the desired name of the adat file to be written. Typically all extensions should be \code{"*.adat"}.
}
  \item{parent}{If known, the original (parent) data frame used to create the data frame to be written. If \code{dat} has \emph{not} been modified and has intact attributes, \code{parent=dat} or \code{parent=NULL} can be used.
}
}

\details{
The end of line (EOL) character affects the format of the resulting file, particularly in calculating the checksum, and is platform dependent. The EOL format is determined by the internal function \code{get.eol} (via \code{R.version$os}), which detects the current operating system and makes appropriate modifications to the EOL default. EOL encoding is below:

\tabular{llc}{
	Symbol \tab Platform \tab Character \cr
	CRLF \tab DOS/Windows \tab "\\r\\n" \cr
	LF \tab UNIX \tab "\\n" \cr
	CR \tab Macintosh \tab "\\r"
}
}

\value{
A tab delimited "*.adat" file written to the current ADAT specification.
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
\code{\link{read.adat}} \cr
\code{\link{write.table}} \cr
\code{\link{verify.adat.checksum}}
}

\examples{
\dontrun{
write.adat(sample.adat, file="test.adat", parent=sample.adat)
write.adat(sample.adat, file="test.adat")		# same as above; self is parent
}
}

