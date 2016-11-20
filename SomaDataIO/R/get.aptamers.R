# --------------------
# Revision Info
# --------------------
# $Revision: 16279 $
# $Author: sfield $
# $LastChangedDate: 2015-03-30 13:00:08 -0600 (Mon, 30 Mar 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/get.aptamers.R $
#######################################
#			Function:	get.aptamers
#######################################
get.aptamers <- get.somamers <- function(adat, remove.controls=FALSE, n=FALSE) {
	# covers a vector of names passed 
	# this function officially superceded get.aptamers.list() on 02/02/2015
	if ( is.null(dim(adat)) && is.class(adat,'character') ) {
		adat <- as.character(adat)
		q <- seq.regex(adat)
		apts <- adat[ q >= 0 ]
	}
	else if ( is.class(adat,'matrix') ) {
		q <- seq.regex(colnames(adat))
		apts <- colnames(adat)[ q >= 0 ]
	}
	else if ( is.class(adat, 'data.frame') ) {
		q <- seq.regex(names(adat))
		apts <- names(adat)[ q >= 0 ]
	}
	else
		stop("Problem with format of 'adat' argument ... must be either a character vector, data frame, or named matrix")


	if ( remove.controls )
		#apts <- apts[ grep('^NonBiotin|^Spuriomer', apts, invert=TRUE) ]
		apts <- apts[ grep('^NonHuman|^NonBiotin|^Spuriomer', apts, invert=TRUE, ignore.case=TRUE) ]

	if ( n )
		length(apts)
	else
		apts

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2013-07-18 12:20:33
