# --------------------
# Revision Info
# --------------------
# $Revision: 14917 $
# $Author: sfield $
# $LastChangedDate: 2014-05-07 17:26:20 -0600 (Wed, 07 May 2014) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/is.seq.R $
#######################################
#			Function:	is.seq
#######################################
is.seq <- function(x) grepl("^[0-9]{4,5}[-.][0-9]{1,3}[_][0-9]{1,3}$", x)
#### ---- END FUNCTION ---- ####

# ---- saved on: 2014-01-21 08:47:05