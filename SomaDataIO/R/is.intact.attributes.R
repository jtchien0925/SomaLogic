# --------------------
# Revision Info
# --------------------
# $Revision: 16374 $
# $Author: sfield $
# $LastChangedDate: 2015-04-17 09:50:21 -0600 (Fri, 17 Apr 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaGlobals/R/is.intact.attributes.R $
#######################################
#			Function:	is.intact.attributes
#######################################
is.intact.attributes <- function(adat, verbose=getOption("verbose")) {

	atts <- attributes(adat)

	#col.meta.checks <- c("SeqId","Dilution","Target","Units","EntrezGeneSymbol")
	col.meta.checks <- c("Dilution","Target","Units")		# this may need updating for older adats

	if ( length(names(atts)) <= 3 ) {
		if (verbose) cat("Attributes has only 3 entries\n")
		return(FALSE)
	}
	else if ( any(!c("Header.Meta","Col.Meta") %in% names(atts)) ) {
		if (verbose) cat("Header.Meta and/or Col.Meta missing\n")
		return(FALSE)
	}
	else if ( any(!c("HEADER","COL_DATA","ROW_DATA") %in% names(atts$Header.Meta)) ) {
		if (verbose) cat(sprintf("Header.Meta missing: %s\n", c("HEADER, COL_DATA, or ROW_DATA")))
		return(FALSE)
	}
	else if ( any(!col.meta.checks %in% names(atts$Col.Meta)) ) {
		if (verbose) cat(sprintf("Col.Meta missing: %s\n", paste(col.meta.checks, collapse=', ')))
		return(FALSE)
	}
	else
		return(TRUE)

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2013-12-13 10:49:53
