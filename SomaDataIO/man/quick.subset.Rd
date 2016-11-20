\name{quick.subset}
\alias{quick.subset}

\title{
Quickly Subset an Data Frame
}

\description{
Easily and quickly subsets rows/samples and columns/features of a data frame based on the \code{list} passed via the \code{\dots} argument.
}

\usage{
quick.subset(df, keep.cols=NULL, ...)
}

\arguments{
  \item{df}{
The data frame to subset, typically a \code{"soma.adat"}.
}
  \item{keep.cols}{
Character. An (optional) list of \emph{columns} keep. Defaults to \code{NULL}, which keeps all columns.
}
  \item{\dots}{
Arguments passed via \dots are used for subsetting \emph{rows} and should follow the format: \code{COLUMN_NAME="Match"}. For example, to restrict the data frame to only those samples in the "Disease" column with a value of "No" type: \code{list(Disease="No")}. Multiple column matches are allowed.
}
}

\details{
The actual sub-setting is performed by arguments passed via the \code{\dots} argument. See \bold{Examples} section below for format and syntax.
}

\value{
A sub-setted data frame. New dimensions are printed to the console.

\bold{Note:} Changing the number of \emph{columns} will strip the attributes of the data frame, resulting in the loss of "Header.Meta" or "Col.Meta" in the attributes of the "soma.adat" object.
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
\code{\link[base]{subset}}
}

\examples{
test <- quick.subset(sample.adat, SampleType="Calibrator")	# calibrators only
test2 <- quick.subset(sample.adat, SampleType="Sample")		# clinical samples
test3 <- quick.subset(sample.adat, SampleGroup=c("M","F"))	# either "M" or "F" as gender (calibrators are "")
test4 <- quick.subset(sample.adat, SampleGroup="M", TimePoint="Old")		# 2 field subset
}

