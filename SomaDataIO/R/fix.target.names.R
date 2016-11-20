######################################### 
#			Function:	fix.target.names
######################################### 
fix.target.names <- function(target.vec, types, replace.empty) {

	indices <- grep("Spuriomer", types)
	target.vec[indices] <- "Spuriomer"
	
	indices <- grep("ybrid.*Block$", types, ignore.case=TRUE)
	target.vec[indices] <- "HybControlBlock"

	indices <- grep("ybrid.*Elution$", types, ignore.case=TRUE)
	target.vec[indices] <- "HybControlElution"

	indices <- grep("^Non[\\.-]human$", types, ignore.case=TRUE)
	target.vec[indices] <- "NonHuman"
	target.vec <- gsub("^non[\\.-]human$", "NonHuman", target.vec, ignore.case=TRUE)
	
	indices <- grep("^Non-Cleavable", types, ignore.case=TRUE)
	target.vec[indices] <- "NonCleavable"

	indices <- grep("Biotin$", types, ignore.case=TRUE)
	target.vec[indices] <- "NonBiotin"

	which.empty <- grep('.', target.vec, invert=TRUE)
	if ( length(which.empty) > 0 )
		target.vec[which.empty] <- replace.empty		# fix for empty strings in target names
	
	assign("empty.logic", length(which.empty)>0, envir=parent.frame())
	target.vec

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2015-03-10 14:53:03
