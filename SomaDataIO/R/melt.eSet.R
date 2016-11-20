# --------------------
# Revision Info
# --------------------
# $Revision: 16519 $
# $Author: sfield $
# $LastChangedDate: 2015-05-21 10:32:18 -0600 (Thu, 21 May 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaDataIO/R/melt.eSet.R $
################################## 
#			Function:	melt.eSet
################################## 
melt.eSet <- function(eSet) {
	# Convert ExpressionSet object to long format merging in both feature and sample data

	if ( !requireNamespace("reshape", quietly=TRUE) )
		stop('The "reshape" package required for this function. Please install via:\n  install.packages("reshape")', call.=FALSE)

	dat.long <- melt(t(exprs(eSet)))			# samples (rows) x features (cols)
	colnames(dat.long) <- c("sample.id","feature","value")

	dat.long <- merge(data.frame(feature=rownames(fData(eSet)),
										  fData(eSet),
										  stringsAsFactors=FALSE),
							dat.long, by="feature")

	dat.long <- merge(data.frame(sample.id=rownames(pData(eSet)),
										  pData(eSet),
										  stringsAsFactors=FALSE),
							dat.long, by="sample.id")
	dat.long
}
#### ---- END FUNCTION ---- ####

# ---- created on: 2015-03-09 15:50:38
