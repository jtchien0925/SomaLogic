# --------------------
# Revision Info
# --------------------
# $Revision: 16265 $
# $Author: sfield $
# $LastChangedDate: 2015-03-25 10:20:55 -0600 (Wed, 25 Mar 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/make.numeric.R $
##################################### 
#			Function:	make.numeric
##################################### 
make.numeric <- function(x, y=NULL) {
	# x = a data frame or list
	# y = character string of colnames in x to convert to numeric
	stopifnot(is.class(x,'data.frame') || is.class(x,'list'))

	if ( is.null(y) ) {
		out <- lapply(x, function(col) {
						  vec <- suppressWarnings(as.numeric(col))
						  if ( sum(is.na(vec))/length(vec) > 0.2 )	# > 20% NAs keep as string
							  gsub("^ *","",col)								# if string, zap leading whitespace
						  else 
							  vec
						})
		if ( is.class(x,"data.frame") )
			out <- as.data.frame(out, row.names=rn(x), stringsAsFactors=FALSE)
	} else {
		if ( !all(y %in% names(x)) )
			stop("Entry(s) exist in 'y' not in colnames of 'x'")
		out <- x			# work with copy
		for ( i in y ) { 
			vec <- suppressWarnings(as.numeric(as.character(out[[i]]))) 
			if ( sum(is.na(vec))/length(vec) < 0.2 )
				out[[i]] <- vec
		}
	}

	return(out)

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2014-10-09 15:31:53
