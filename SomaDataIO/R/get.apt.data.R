# --------------------
# Revision Info
# --------------------
# $Revision: 16468 $
# $Author: sfield $
# $LastChangedDate: 2015-05-08 09:35:07 -0600 (Fri, 08 May 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/get.apt.data.R $
#######################################
#       Function: get.apt.data
#######################################
get.apt.data <- get.soma.data <- function(adat) {

	if ( "Col.Meta" %in% names(attributes(adat)) ) {
		apt.data <- as.data.frame(attributes(adat)$Col.Meta, stringsAsFactors=FALSE)
		if ( "Dilution" %in% names(apt.data) )
			apt.data$Dilution2 <- as.numeric(gsub("%$|Mix ","",apt.data$Dilution))
	} else
		stop("  No SOMAmer data found in the attributes.\n  The attributes may have been stripped if/when the data.frame object was modified.")

	# determine row names
	if ( nrow(adat) > 0 ) 
		row.names(apt.data) <- get.aptamers(adat)
	else {
		label = 
		if ( "EntrezGeneSymbol" %in% names(apt.data) )
			apt.data$EntrezGeneSymbol
		else if ( "Target" %in% names(apt.data) )
			apt.data$Target
		else
			stop("Neither EntrezGeneSymbol nor Target found in Col.Meta block of ADAT attributes")

		id = 
		if ( "SeqId" %in% names(apt.data) )
			apt.data$SeqId
		else if ( "SomaId" %in% names(apt.data) )
			apt.data$SomaId
		else
			stop("Neither SeqId nor SomaId found in Col.Meta block of ADAT attributes")

		new.rn <- clean.names(paste(label, id, sep="."))	# zap non-alphanum

		if ( length(new.rn) > 0 )
			row.names(apt.data) <- new.rn
	}

	char <- c("SeqId","SomaId","Target","TargetFullName",
				 "UniProt","EntrezGeneID","EntrezGeneSymbol",
				 "Organism","Type","Dilution","Units","ColCheck")

	names(apt.data) <- clean.names(names(apt.data))
	nums <- setdiff(names(apt.data), char)

	if ( length(nums) > 0 )
		make.numeric(apt.data, nums)
	else
		apt.data

}
#######------- END FUNCTION -------#######
