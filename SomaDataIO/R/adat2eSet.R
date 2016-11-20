# --------------------
# Revision Info
# --------------------
# $Revision: 16411 $
# $Author: sfield $
# $LastChangedDate: 2015-04-22 09:52:35 -0600 (Wed, 22 Apr 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/adat2eSet.R $
################################## 
#			Function:	adat2eSet
################################## 
adat2eSet <- function(adat) {
	# Convert ADAT to a Bioconductor ExpressionSet object

	if ( !requireNamespace("Biobase", quietly=TRUE) )
		stop("Biobase package required for this function.\n  See help(adat2eSet) for installation instructions", call.=FALSE)

	stopifnot( is.intact.attributes(adat) )
	atts <- attributes(adat)
	class(adat) <- "data.frame"
	lst <- list()
	lst$fdata <- data.frame(atts$Col.Meta, stringsAsFactors=FALSE)
	rownames(lst$fdata) <- clean.names(paste(lst$fdata$EntrezGeneSymbol,lst$fdata$SeqId,sep='.'))
	lst$pdata <- adat[, get.meta(adat) ]
	lst$header <- atts$Header.Meta$HEADER
	lst$exprs <- adat[, get.aptamers(adat) ] %names% rownames(lst$fdata)

	fdata <- new("AnnotatedDataFrame",
					 data=lst$fdata,
					 varMetadata=data.frame(labelDescription=gsub("\\."," ",colnames(lst$fdata)),
													row.names=colnames(lst$fdata)) )

	pdata <- new("AnnotatedDataFrame",
					 data=lst$pdata,
					 varMetadata=data.frame(labelDescription=gsub("\\."," ",colnames(lst$pdata)), row.names=colnames(lst$pdata)) )

	eset <- ExpressionSet(t(as.matrix(lst$exprs)),
								 varMetadata=data.frame(labelDescription=gsub("_"," ",colnames(pdata))),
								 featureData=fdata,
								 phenoData=pdata)
		
	experimentData <- experimentData(eset)
	experimentData@title <- lst$header$Title
	experimentData@url <- lst$header$DerivedFrom
	experimentData@other <- c(list(R.version=sessionInfo()$R$version.string,
											 R.platform=sessionInfo()$R$platform,
											 R.arch=sessionInfo()$R$arch),
									  lst$header,
									  list(processingDateTime=as.character(Sys.time()))
									  )
	experimentData(eset) <- experimentData
	
	if ( !validObject(eset) )
		stop("The ExpressionSet object was created but it is not valid!")
	
	return(eset)

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2015-03-09 15:53:18
