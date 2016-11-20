# --------------------
# Revision Info
# --------------------
# $Revision: 15966 $
# $Author: sfield $
# $LastChangedDate: 2015-02-05 11:22:02 -0700 (Thu, 05 Feb 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/child.atts.wrapper.R $
#######################################
#       Function: child.atts.wrapper
#######################################
#    Combines create.child.attributes & get.parent.attributes
#    Used in:  write.adat & write.ss.adat
######################################################################
child.atts.wrapper <- function(child, parent=NULL) {
   
	if ( is.intact.attributes(child) )
		return(attributes(child))

	if ( is.null(parent) ) {
		cat('*  Child adat orphaned ... trying to locate a parent ...\n')
		p.atts <- get.parent.attributes(df=child)
	}
	else
		p.atts <- attributes(parent)

	create.child.attributes(child.df=child, parent.atts=p.atts, verbose=FALSE)

}
#### ---- END FUNCTION ---- ####

# ---- created on 2013-01-31 12:25:23
