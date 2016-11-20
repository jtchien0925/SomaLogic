# --------------------
# Revision Info
# --------------------
# $Revision: 16211 $
# $Author: sfield $
# $LastChangedDate: 2015-03-16 16:57:10 -0600 (Mon, 16 Mar 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/refactor.data.R $
#######################################
#       Function: refactor.data
#######################################
refactor.data <- function(data) {
	for ( meta in get.meta(data) )
		if ( is.factor(data[[meta]]) )
			data[[meta]] <- droplevels(data[[meta]])
	data
}
#######------- END FUNCTION -------#######
