# --------------------
# Revision Info
# --------------------
# $Revision: 16558 $
# $Author: sfield $
# $LastChangedDate: 2015-06-05 10:50:58 -0600 (Fri, 05 Jun 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/parse.apt.R $
################################## 
#			Function:	parse.apt
################################## 
parse.apt <- function(apt) {
	# pre-processing for increasing complex aptamer names (mostly gene names with numbers) sgf

	apt2 <- gsub("\\.{2,3}",".",apt)		# zap double or triple dots in name (should no longer be necessary as zapped on read-in
	char.split <- strsplit(apt2, split="\\.|-|_", fixed=FALSE)[[1]]
	char <- grep("[A-Za-z]", char.split, value=TRUE, invert=TRUE)		# only numerics
	paste(char[1:2],collapse=".")													# strip minor version & put back together

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2014-07-30 10:03:03
