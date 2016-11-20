# --------------------
# Revision Info
# --------------------
# $Revision: 16265 $
# $Author: sfield $
# $LastChangedDate: 2015-03-25 10:20:55 -0600 (Wed, 25 Mar 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/get.meta.R $
#######################################
#			Function:	get.meta
#######################################
get.meta <- function(adat, n=FALSE) {
	# covers a vector of names passed 
	# this function officially superceded get.non.aptamers.list() on 02/03/2015

	if ( is.null(dim(adat)) && is.class(adat,'character') ) {
		adat <- as.character(adat)
		q <- seq.regex(adat)
		vec <- adat[ q < 0 ]
	}
	else if ( is.class(adat,'matrix') ) {
		q <- seq.regex(colnames(adat))
		vec <- colnames(adat)[ q < 0 ]
	}
	else if ( is.class(adat, "data.frame") ) {
		q <- seq.regex(names(adat))
		vec <- names(adat)[ q < 0 ]
	}
	else
		stop("Problem with format of 'adat' argument ... must be either a character vector, data frame, or named matrix")

	if ( n )
		length(vec)
	else
		vec

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2013-09-05 15:27:31
