# --------------------
# Revision Info
# --------------------
# $Revision: 16019 $
# $Author: sfield $
# $LastChangedDate: 2015-02-06 18:16:29 -0700 (Fri, 06 Feb 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/writer.prep.R $
#######################################
#       Function:		writer.prep
#######################################
writer.prep <- function(dat, file, parent) {

	if ( missing(dat) )
		stop("Adat object is missing")
	if ( missing(file) )
		stop("Must provied output file name ... e.g. 'my_data_output.adat'")
	if ( is.null(parent) && !is.intact.attributes(dat) )
	  stop("\n  It looks like you are trying to write an adat with broken attributes\n  In this case, you *MUST* pass a parent wtih intact attributes via 'parent=' in order to reconstruct the attributes")
	if ( !is.null(parent) && !is.intact.attributes(parent) ) {
		if ( is.class(parent, 'list') )
			stop("Are you sure you are passing the parent adat (not the attributes)?")
		stop("\n  Parent failed 'intact.attributes' check\n  You may be using an out of spec or old adat, or the parent has been modified.\n  Please reload using the current version of read.adat() or load.adat()")
	}

	# --------------------- #
	# Move Response column
	# --------------------- #
	if ( 'Response' %in% names(dat) ) {
		dat <- dat[ , c( get.meta(dat), get.aptamers(dat)) ]
		cat('*  Moving "Response" column to end of meta data prior to writing adat ...\n')
	}

	norms <- grep('NormScale', names(dat))

	if ( length(norms) > 1 )
		names(dat)[norms] <- gsub('[.]{2,4}', '.', names(dat)[norms])

	dat.atts <- child.atts.wrapper(child=dat, parent=parent)
	dat.atts <- fix.header.meta(dat.atts)
	dat.atts$Col.Meta <- lapply(dat.atts$Col.Meta, function(row) gsub(',',';', row))		# zap commas in all Col.Meta

	# ---- checks & traps ---- #
	attributes(dat) <- dat.atts
	check.adat(dat)

	return(dat)

}


