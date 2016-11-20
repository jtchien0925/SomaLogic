\name{verify.adat.checksum}
\alias{verify.adat.checksum}
\alias{checksum}

\title{
Verify an ADAT Checksum
}

\description{
Confirms that the Checksum written in line 1 of a standard "*.adat" file actually corresponds to the Checksum calculated from remainder of the file.
}

\usage{
verify.adat.checksum(file)
}

\arguments{
  \item{file}{
An "*.adat" file containing a Checksum line.
}
  %\item{eol}{The end of line (EOL) character(s) to use when calculating a file's checksum. For example, \code{eol = "\r\n"} will produce Windows' line endings, \code{eol = "\n"} will produce UNIX line endings, and \code{eol="\r"} will produce Macintosh line endings.}
}

\details{
Checksum is calculated using the secure hex algorithm 1 (SHA1) and does \emph{not} include the Checksum line of an "*.adat" file. \bold{Note:} by default, the checksum is calculated based on the DOS/Windows end of line (EOL) character ("\\r\\n").
}

\value{
\code{TRUE} if the checksum in line 1 of the "*.adat" file matches the calculated checksum for the remainder of the file. Otherwise \code{FALSE}.
}

\references{
SHA1 references: \cr
http://www.itl.nist.gov/fipspubs/fip180-1.htm \cr
http://en.wikipedia.org/wiki/SHA_hash_functions 
}

\author{
Stu Field
}

\note{
This function depends upon the \code{\link[digest]{digest}} package.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
\dontrun{
verify.adat.checksum("path/to/file.adat")
}
}

\keyword{ SHA1 }
\keyword{ MD5 }
