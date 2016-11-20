# --------------------
# Revision Info
# --------------------
# $Revision: 16268 $
# $Author: sfield $
# $LastChangedDate: 2015-03-25 16:55:29 -0600 (Wed, 25 Mar 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaDataIO/R/attach.soma.data.R $
#######################################
#			Function:	attach.soma.data
#######################################
attach.soma.data <- function(adat) {
	if ( !is.intact.attributes(adat) )
		stop("Attributes of 'adat' not intact ... has the object been modified?")
	.GlobalEnv$somamers <- get.aptamers(adat)
	.GlobalEnv$somamer.list <- get.apt.names(adat)
	ad <- get.apt.data(adat)
	.GlobalEnv$somamer.table <- ad
	.GlobalEnv$target.names <- get.target.names(ad)
}
#### ---- END FUNCTION ---- ####
