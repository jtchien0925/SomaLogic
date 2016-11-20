# --------------------
# Revision Info
# --------------------
# $Revision: 16162 $
# $Author: sfield $
# $LastChangedDate: 2015-03-10 10:20:30 -0600 (Tue, 10 Mar 2015) $
# $URL: $
########################################## 
#			Function:	get.pdat.rownames
########################################## 
get.pdat.rownames <- function(pdat) {

	# Agilent
	if ( "SlideId.Subarray" %in% names(pdat) )
		row.names(pdat) <- pdat$SlideId.Subarray
	else if ( all(c("Subarray","SlideId") %in% names(pdat)) ) {
		pdat.rownames <- paste(pdat$SlideId, pdat$Subarray, sep="_")
		if ( any(duplicated(pdat.rownames)) ) {
			if ( "DatasetId" %in% names(pdat) )		# Added for datasets with same slide_id sub scanned with different software
				pdat.rownames <- paste(pdat$DatasetId, pdat.rownames, sep="_")
			if ( any(duplicated(pdat.rownames)) ) {
				cat('*  Found duplicate row names (SlideId_Subarray non-unique) ...\n')
				stop(pdat.rownames[duplicated(pdat.rownames)])
			}
		}
	}
	# Luminex
	else if ( "PlateId" %in% names(pdat) && "WellId" %in% names(pdat)) {
		pdat.rownames <- paste(pdat$PlateId, pdat$WellId, sep="_")
		if ( any(duplicated(pdat.rownames)) ) {
			cat('*  Found duplicate row names (SlideId_Subarray non-unique) ...\n')
			stop(pdat.rownames[duplicated(pdat.rownames)])
		}
	}
	else {
		warning("No SlideId_Subarray found in ADAT ... row names will be numbered by row #")
		pdat.rownames <- as.character(seq(nrow(pdat)))
	}

	pdat.rownames

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2015-03-08 14:15:48
