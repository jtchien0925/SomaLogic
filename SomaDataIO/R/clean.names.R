# --------------------
# Revision Info
# --------------------
# $Revision: 16162 $
# $Author: sfield $
# $LastChangedDate: 2015-03-10 10:20:30 -0600 (Tue, 10 Mar 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/clean.names.R $
#################################### 
#			Function:	clean.names
#################################### 
clean.names <- function(x) {

	x <- gsub(" *$", "", x)							# rm trailing whitespace
  	x <- gsub("[^A-Za-z0-9]", ".", x)			# zap non-alphanum names
  	x <- gsub("[.]{2,5}", ".", x)					# zap double/triple dots
	x <- gsub("^Hyb[.]Scale", "HybControlNormScale", x)
	x <- gsub("^Med.Scale","NormScale", x)
	x <- gsub("^[.]", "", x)						# zap leading dots
	return(x)

}
#### ---- END FUNCTION ---- ####
