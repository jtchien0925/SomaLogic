# --------------------
# Revision Info
# --------------------
# $Revision: 14917 $
# $Author: sfield $
# $LastChangedDate: 2014-05-07 17:26:20 -0600 (Wed, 07 May 2014) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaGlobals/R/names.R $
###################################
#	general purpose operator
###################################
'%names%' <- function(obj, ex) { names(obj) = ex; obj }
###################################
