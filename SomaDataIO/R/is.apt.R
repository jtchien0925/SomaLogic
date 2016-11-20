# --------------------
# Revision Info
# --------------------
# $Revision: 15599 $
# $Author: sfield $
# $LastChangedDate: 2014-11-03 18:03:26 -0700 (Mon, 03 Nov 2014) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaGlobals/R/is.apt.R $
#######################################
#       Function: is.apt
#######################################
is.apt <- function (x) grepl("[0-9]{4,5}[-.][0-9]{1,3}[_.][0-9]{1,3}", x)
#### ---- END FUNCTION ---- ####

# ---- saved on 2012-09-11 17:13:51
