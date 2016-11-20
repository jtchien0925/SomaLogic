# --------------------
# Revision Info
# --------------------
# $Revision: 16533 $
# $Author: apoole $
# $LastChangedDate: 2015-05-27 13:19:59 -0600 (Wed, 27 May 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaGlobals/R/seq.regex.R $
#######################################
#			Function:	seq.regex
#######################################
seq.regex <- function(x) { regexpr('[0-9]{4,5}[-.][0-9]{1,3}([._][0-9]{1,2})?$|^SL[0-9]{6}$', x) }
#### ---- END FUNCTION ---- ####

# ---- saved on: 2013-12-17 13:28:58
