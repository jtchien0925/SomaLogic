# --------------------
# Revision Info
# --------------------
# $Revision: 16265 $
# $Author: sfield $
# $LastChangedDate: 2015-03-25 10:20:55 -0600 (Wed, 25 Mar 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/quick.subset.R $
##################################### 
#			Function:	quick.subset
##################################### 
quick.subset <- function(df, keep.cols=NULL, ...) {

	orig.dims <- dim(df)
	orig.cl <- class(df)
	args <- list(...)
	 
	if ( !all(c(names(args),keep.cols) %in% names(df)) )
		stop("Cannot subset! Names mismatch in data frame ... please check spelling")

	for ( n in names(args) ) {
		df <- df[ df[[n]] %in% args[[n]], ]		# subset rows
	}

	if ( nrow(df)==0 )
		stop("No rows left in data frame after subsettng")

	df <- refactor.data(df)							# remove ghost levels
	tmp.rn <- rn(df)

	if ( !is.null(keep.cols) )
		df <- df[ , keep.cols ]

	if ( length(keep.cols) == 1 ) {
		df <- data.frame(df, stringsAsFactors=FALSE) %names% keep.cols
		rownames(df) <- tmp.rn
		class(df) <- orig.cl
	}

   cat('*  Original dims ...', paste(orig.dims, collapse=' x '), '\n')
	cat('*  New dims ........', paste(dim(df), collapse=' x '), '\n')

	return(df)

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2014-05-07 13:27:00
