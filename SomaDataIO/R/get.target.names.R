# --------------------
# Revision Info
# --------------------
# $Revision: 15709 $
# $Author: sfield $
# $LastChangedDate: 2014-11-26 19:27:28 -0700 (Wed, 26 Nov 2014) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/get.target.names.R $
#######################################
#			Function:	get.target.names
#######################################
get.target.names <- function(apt.data=NULL) {

	if ( !'apt.data'%in%ls(.GlobalEnv) && is.null(apt.data) )
		stop("apt.data must be defined in global namespace or you must provide it")

	if ( is.null(apt.data) && 'apt.data'%in%ls(.GlobalEnv) ) {
		warning('apt.data retreived from .GlobalEnv')
		apt.data <- get('apt.data', envir=.GlobalEnv)
	}

	col <- if ( 'TargetFullName'%in%names(apt.data) ) 'TargetFullName' else 'Target'
	lapply(rn(apt.data), function(x) apt.data[x,col]) %names% rn(apt.data)

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2014-01-10 12:10:12
