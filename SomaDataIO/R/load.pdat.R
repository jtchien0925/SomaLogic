# --------------------
# Revision Info
# --------------------
# $Revision: 16264 $
# $Author: sfield $
# $LastChangedDate: 2015-03-24 21:17:52 -0600 (Tue, 24 Mar 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/load.pdat.R $
#######################################
#       Function: load.pdat
#######################################
load.pdat <- function(file, ...) {

	pdat <- read.delim(file, row.names=NULL, comment.char="", as.is=TRUE, ...)
	row.names(pdat) <- get.pdat.rownames(pdat)
	num.cols <- ncol(pdat)

	# Java pdats have an extra blank column at end; fixed here
	# I don't think this will work anymore; sgf 
	# print(names(pdat))
	if ( names(pdat)[num.cols]=="X" ) {
		cat('*  Warning: Removing final column with no header, please check table\n')
		pdat <- pdat[ ,1:(num.cols-1) ]
	}

	# Integer to float fix
	for ( name in names(pdat) ) {
		if ( is.integer(pdat[,name]) )
			pdat[,name] <- as.numeric(pdat[,name])
	}
	
	return(pdat)

}
#######------- END FUNCTION -------#######
