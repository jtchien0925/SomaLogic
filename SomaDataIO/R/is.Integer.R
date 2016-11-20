# --------------------
# Revision Info
# --------------------
# $Revision: 16374 $
# $Author: sfield $
# $LastChangedDate: 2015-04-17 09:50:21 -0600 (Fri, 17 Apr 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaGlobals/R/is.Integer.R $
################################### 
#			Function:	is.Integer
################################### 
is.Integer <- function(x, verbose=getOption("verbose")) {
	if ( !is.numeric(x) ) {
		if ( verbose )
			cat('*  Vector is of class:', class(x), '\n')
		return(FALSE)
	}
	all(floor(x)==x, na.rm=TRUE)
}
#### ---- END FUNCTION ---- ####

