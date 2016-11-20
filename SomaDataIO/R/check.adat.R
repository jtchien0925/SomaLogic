# --------------------
# Revision Info
# --------------------
# $Revision: 15966 $
# $Author: sfield $
# $LastChangedDate: 2015-02-05 11:22:02 -0700 (Thu, 05 Feb 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/check.adat.R $
#######################################
#       Function: check.adat
#######################################
check.adat <- function(adat) {

	atts <- attributes(adat)
	meta <- get.meta(adat)

	if ( !is.intact.attributes(adat) )
		stop("Incomplete attributes: was the data frame modified?")

	if ( !all(names(atts$Col.Meta) == atts$Header.Meta$COL_DATA$Name) )
		stop("Col.Meta data and Header.Meta$COL_DATA mismatch: check names of Column Meta data")

	if ( !all(meta == atts$Header.Meta$ROW_DATA$Name) )
		stop("Header.Meta$ROW_DATA and adat meta data mismatch: check attributes(adat)$Header.Meta$ROW_DATA$Name")

	if ( get.aptamers(adat, n=TRUE) != nrow(as.data.frame(atts$Col.Meta)) )
		stop("Number of aptamers in adat does not match No. aptamers in Col.Meta")

	if ( nrow(adat)==0  )
		warning('Adat has no rows! Writing just header and column meta data')

	cat(sprintf('*  Adat passed checks and traps ...... TRUE\n'))

}

#### ---- END FUNCTION ---- ####

# ---- created on 2013-12-18 18:39:56

