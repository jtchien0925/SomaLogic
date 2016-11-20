# --------------------
# Revision Info
# --------------------
# $Revision: 16531 $
# $Author: sfield $
# $LastChangedDate: 2015-05-27 11:07:59 -0600 (Wed, 27 May 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/diff.check.cols.R $
#######################################
#			Function:	diff.check.cols
#######################################
diff.check.cols <- function(x, y, meta=FALSE, tolerance) {

	.fun <- if ( meta ) get.meta else get.aptamers
	cols <- intersect(.fun(x), .fun(y))

	test.col <- sapply(cols, function(.col) {
							 if ( meta )
								 isTRUE(all.equal(as.character(x[,.col]), as.character(y[,.col]), check.names=FALSE)) 
							 else
								 isTRUE(all.equal(x[,.col], y[,.col], tolerance=tolerance))
					}) %names% cols

	# ***test.col is a logical vector***

	type <- ifelse(meta,"Meta","Feature")
	dots <- switch(type, Meta="...............", Feature="............")

	if ( all(test.col,na.rm=TRUE) ) {
		cat(sprintf("*  All %s data is identical %s TRUE\n", type, substring(dots,5)))
		"Identical"
	} else {
		cat(sprintf("*  %s data is identical %s FALSE\n", type, dots))
		vec <- names(test.col)[ !test.col ]
		cat(sprintf("*  %s data fields with differing values among adats %s %i\n", type, substring(dots,5), length(vec)))
		vec
	}
}
#### ---- END FUNCTION ---- ####

# ---- created on: 2013-11-08 13:11:38
