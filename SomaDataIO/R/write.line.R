# --------------------
# Revision Info
# --------------------
# $Revision: 14917 $
# $Author: sfield $
# $LastChangedDate: 2014-05-07 17:26:20 -0600 (Wed, 07 May 2014) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaGlobals/R/write.line.R $
#######################################
#       Function: write.line
#######################################
write.line <- function(sprintf.str, ...) {
	cat(paste(sprintf(sprintf.str, ...), "\n", sep=""))
}
#######------- END FUNCTION -------#######
