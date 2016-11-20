# --------------------
# Revision Info
# --------------------
# $Revision: 16510 $
# $Author: sfield $
# $LastChangedDate: 2015-05-20 17:59:23 -0600 (Wed, 20 May 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaGlobals/R/vec.diff.R $
#######################################
#			Function:	vec.diff
#######################################
vec.diff <- function(x, y, verbose=getOption("verbose")) {

	x.y.diff = setdiff(x,y)
	y.x.diff = setdiff(y,x)
	xy.inter = intersect(x,y)
	xy.union = union(x,y)

	if ( verbose ) {
		write.line("  Unique to first ..... %i", length(x.y.diff))
		write.line("  Unique to second .... %i", length(y.x.diff))
		write.line("  Common .............. %i", length(xy.inter))
		write.line("  Union ............... %i", length(xy.union))
	}

	invisible(list(first.only=x.y.diff, second.only=y.x.diff,
						inter=xy.inter, unique=xy.union))

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2013-11-20 15:16:32
