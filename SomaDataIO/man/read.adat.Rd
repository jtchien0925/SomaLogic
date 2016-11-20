\name{read.adat}
\alias{read.adat}
\alias{load.adat}
\alias{load.legacy}
\alias{print.soma.adat}
\alias{summary.soma.adat}

\title{
Read (Load) SomaLogic ADATs
}

\description{
Parses and loads a \code{"*.adat"} file as a \code{data.frame} object into an R workspace. The class of this new object is a "soma.adat" class object. 
}

\usage{
read.adat(file, protein.label="EntrezGeneSymbol", remove.buffer=TRUE, replace.names=TRUE,
          verbose=getOption("verbose"), debug=FALSE, ...)

load.adat(file, protein.label="EntrezGeneSymbol", remove.buffer=TRUE, replace.names=TRUE,
          verbose=getOption("verbose"), debug=FALSE, ...)

\method{print}{soma.adat}(x, ...)

\method{summary}{soma.adat}(object, soma.data=NULL, digits=max(3L, getOption("digits") - 3L), ...)
}

\arguments{
  \item{file}{
Character. The elaborated path and file name of the "*.adat" file to be loaded into an R workspace. If it does not contain an \emph{absolute} path, the file name is \emph{relative} to the current working directory, \code{\link[base]{getwd}}.
}
  \item{protein.label}{
A character string corresponding to a column of meta data. Typical options are "EntrezGeneSymbol", "Target", or "GeneName". The labels from one of these rows of column meta data will be combined with the SOMAmer identifier (typically "SeqId"), to form a unique ID to label the \code{data.frame} columns.
}
  \item{remove.buffer}{
Logical. Should buffer sample(s), if present, be removed during loading?
}
  \item{replace.names}{
Logical. Should user friendly names be created for the SomaLogic control feature analytes (e.g. Spuriomers, NonHuman, NonBiotin, HybControls)?
}
  \item{verbose}{
Logical. Should the function call be run in \emph{verbose} mode, printing relevant diagnostic call information to the console. Defaults to the global variable defined in \code{\link[base]{getOption}}.
}
  \item{debug}{
Logical. Used for debugging and development of an ADAT that fails to load, particularly out-of-spec, poorly modified, or legacy ADATs.
}
  \item{x}{
Object of class \code{soma.adat} used in the S3 generic \code{\link{print}}.
}
  \item{object}{
Object of class \code{soma.adat} used in the S3 generic \code{\link{summary}}.
}
  \item{soma.data}{
An optional object, the result of a call to \code{\link{get.soma.data}} (or \code{get.apt.data} for backward compatibility), from which Target names can be extracted. If \code{NULL} (default), and the attributes of \code{object} are intact, Target names are extracted from the "Col.Meta" of the attributes. If neither of these options are available, \code{.GlobalEnv} is searched for an object called either \code{"apt.data"} or \code{"soma.data"} and this object is used to extract Target names. If none of the above options are available, the "Target" row of the S3 \code{summary} method is left blank.
}
  \item{digits}{
Integer, used for number formatting with \code{\link{signif}}() (for \code{summary.default}) or \code{\link{format}}() (for \code{summary.data.frame}).
}
  \item{\dots}{
Additional arguments passed ultimately to \code{\link[utils]{read.delim}} (if used within \code{read.adat}) or \code{\link[base]{summary}} (if used within \code{summary}).
}
}

\details{
The \code{load.adat} function is an alias of \code{\link{read.adat}} and is maintained for backward compatibility only. \bold{Note:} some versions of \code{SomaDataIO} do not export \code{load.adat} to the user.
}

\value{
A \code{data.frame} style object of class \code{"soma.adat"} consisting of SomaLogic RFU (feature) data and clinical meta data as columns, and samples as rows. Row names are labeled with the unique ID "SlideId_Subarray" concatenation. The sections of the ADAT header (e.g., "Header.Meta", "Col.Meta", \dots) are stored as object attributes (i.e. attributes(x)$Header.Meta).

The associated S3 generic print method (\code{print}) returns the following, and if available, a summary of the object's attributes (i.e. \code{attributes(x)}):

* Attributes intact ........ (T/F) \cr
* Dimensions:\cr
-----Rows ................. 35 \cr
-----Columns .............. 1149 \cr
-----MetaData ............. 20 \cr
-----FeatureData .......... 1129 \cr
-----Response Present ..... (T/F) \cr
* Header Data: (if present)\cr





The associated S3 generic summary method (\code{summary}) returns the following for each column of the ADAT object containing SOMAmer data (clinical meta data is excluded): 

* Target (if available)\cr
* Minimum value\cr
* 1st Quantile\cr
* Median\cr
* Mean\cr
* 3rd Quantile\cr
* Maximum value\cr
* Standard deviation\cr
}

\references{
%% ~put references to the literature/web site here ~
}

\author{
Stu Field \cr
Michael R. Mehan
}

\note{
}

\seealso{
\code{\link[utils]{read.table}} \cr
\code{\link[base]{attributes}}
}

\examples{
\dontrun{
my.adat <- read.adat("path/to/file.adat")  # read in/load
my.adat   # print
}
mmps <- grep("^MMP",names(sample.adat))  # get MMPs
mmps
sample.adat  # print
summary(sample.adat[,mmps])  # summary of MMPs
}

