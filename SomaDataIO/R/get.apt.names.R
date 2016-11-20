# --------------------
# Revision Info
# --------------------
# $Revision: 16279 $
# $Author: sfield $
# $LastChangedDate: 2015-03-30 13:00:08 -0600 (Mon, 30 Mar 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/get.apt.names.R $
#######################################
#       Function: get.apt.names
#######################################
get.apt.names <- get.soma.names <- function(adat) {
	sapply(get.aptamers(adat), function(apt) apt, USE.NAMES=TRUE, simplify=FALSE)
}
#######------- END FUNCTION -------#######
