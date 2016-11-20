\name{sample.adat}
\alias{sample.adat}
\alias{sample.adat.raw}
\alias{sample.adat.hyb}
\alias{sample.adat.hyb.med}
\alias{sample.adat.hyb.med.cal}
\alias{apt.data_example}
\alias{soma.table_example}
\docType{data}

\title{
Sample Data Frame ADATs
}

\description{
A series of 4 SomaSciences ADAT objects reflecting the 3 normalization stages (hybridization normalization, median normalization, and calibration). The final object is an "somamer.table" object used in many R code scripts, which is the result of a call to \code{get.soma.data}. For convenience, the \code{sample.adat} object is simply a copy of the \code{sample.adat.raw.hyb.med.cal} object, which is typically the final ADAT in the normalization process (for human serum and plasma).
}

\usage{
sample.adat
sample.adat.raw
sample.adat.hyb
sample.adat.hyb.med
sample.adat.hyb.med.cal
soma.table_example
}

\format{
Four adat objects: \cr
1) \bold{sample.adat.raw}: a sample adat containig 27 samples (rows), 1141 SOMAmers (columns), and 20 meta data columns \cr

2) \bold{sample.adat.hyb}: the raw adat above following hybridization normalization \cr

3) \bold{sample.adat.hyb.med}: the hyb adat above following hybridization normalization and median normalization \cr

4) \bold{sample.adat.hyb.med.cal}: the hyb.med adat above following hybridization normalization, median normalization, and calibration \cr

5) \bold{sample.adat}: a copy of sample.adat.hyb.med.cal \cr

6) \bold{soma.table_example}: sample "somamer.table" object from the V3 assay, corresponding to a transposed data frame of the "Col.Meta" of an ADAT.
}

\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}

\source{
Internal SomaLogic reference files: S/PharmaServices/SomaSciences/Example_Tutorial_ADats/example/NormStages
}

\references{ SomaLogic, Inc. }

\examples{
sample.adat
dim(sample.adat)
names(soma.table_example)
head(soma.table_example)
}

\keyword{ datasets }
