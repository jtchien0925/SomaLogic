# --------------------
# Revision Info
# --------------------
# $Revision: 16400 $
# $Author: sfield $
# $LastChangedDate: 2015-04-21 15:49:14 -0600 (Tue, 21 Apr 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/parse.check.R $
#######################################
#			Function:	parse.check
#######################################
parse.check <- function(all.tokens, verbose=getOption("verbose")) {

	if ( is.class(all.tokens,"character") && length(all.tokens)==1 )
		stop("Format is wrong for the tokens argument ... are you sure you are passing a list of TOKENS and not a filename?", call.=FALSE)

	# -----------------------#
	# check if necessary pieces
	# of header present
	# -----------------------#
	firsts <- sapply(all.tokens, head, n=1)
	chk.string <- c("^HEADER","^ROW_DATA","^COL_DATA","^TABLE_BEGIN")
	if ( any(!chk.string %in% firsts) )
		stop("The following landmark tokens is absent from the ADAT header: ", setdiff(chk.string,firsts), call.=FALSE)

	# trailing tabs
	tab.test <- all.tokens[[ which(firsts == "^HEADER") ]]
	if ( length(tab.test) > 100 ) { # catch for runaway tabs
		cat("*  Head:\n")
		print(head(tab.test,10))
		cat("*  Tail:\n")
		print(tail(tab.test,10))
		cat("\n  This does not appear to be a valid ADAT\n  One possibility is that there are empty tabs filling out the entire header block (see above)\n")
	}

	table.begin <- which(firsts == "^TABLE_BEGIN")
	which.col.meta.start <- table.begin + 1
	col.meta.shift <- grep("[A-Za-z0-9]", all.tokens[[ which.col.meta.start ]])[1]

	row.meta <- all.tokens[[ which(firsts =="^ROW_DATA") + 1 ]]
	col.meta <- all.tokens[[ which(firsts =="^COL_DATA") + 1 ]]
	col.meta <- grep(".", col.meta, value=TRUE)
	row.meta <- grep(".", row.meta, value=TRUE)

	if ( col.meta.shift != length(row.meta) )
		stop("The Col.Meta shift (",col.meta.shift,") does not match the length stated in ^ROW_DATA row (",length(row.meta),") -- visually inspect ADAT", call.=FALSE)

	which.col.meta.rows <- seq(which.col.meta.start,which.col.meta.start+length(col.meta)-2,1)

	which.header.row <- table.begin + length(col.meta)

	if ( verbose ) {
		cat(sprintf('\n*  Table Begin ............ %i\n', table.begin))
		cat(sprintf('*  Col Meta Starts ........ %i\n', which.col.meta.start))
		cat(sprintf('*  Col Meta Shift ......... %i\n', col.meta.shift))
		cat(sprintf('*  Header Row ............. %i\n', which.header.row))
		cat(sprintf('*  Rows of the Col Meta ... %s\n', Reduce(paste, which.col.meta.rows)))
		cat(sprintf('*  Col Meta (%i):\n     %s\n', length(col.meta)-1, paste(col.meta[-1], collapse=', ')))
		cat(sprintf('*  Row Meta (%i):\n     %s\n', length(row.meta)-1, paste(row.meta[-1], collapse=', ')))
	}

	if ( col.meta.shift == 1 ) {		# if adat has no feature data, no shift in Col Meta 
		return(list(ColMetaStart=which.col.meta.start,
						HeaderRow=which.header.row,
						ColMetaShift=col.meta.shift))
	}

	ret.list <- list(ColMetaStart=which.col.meta.start,
						  HeaderRow=which.header.row, 
						  ColMetaShift=col.meta.shift)

	if ( which.header.row >= length(all.tokens) )  # if n.tokens < header: no feature data! Only Col.Meta
		return(ret.list)




	# ---------------------------------- #
	# col meta names from Col.Meta block
	# Not from Heater.Meta block
	# ---------------------------------- #
	col.meta2 <- sapply(all.tokens[which.col.meta.start:(which.header.row-1)], function(.x) .x[col.meta.shift])
	#print(col.meta2)

	if ( any(duplicated(col.meta2)) )
		cat(sprintf(" Duplicated Col.Meta names in col meta block!\n   Potential over-write scenario for entry: %s\n\n", paste(col.meta2[duplicated(col.meta2)], collapse=", ")))


	# --------------------- #
	# check col meta match
	# --------------------- #
	if ( !all(sort(col.meta[-1]) == sort(col.meta2)) ) {
		cat('*  Mismatch between ^COL_DATA in header and Col.Meta block:\n')
		cat('*    In Header  :',col.meta[-1],'\n')
		cat('*    In Col.Meta:',col.meta2,'\n')
		stop(call.=FALSE)
	}

	# --------------------- #
	# check row meta match
	# --------------------- #
	string.in.header <- row.meta[-1]
	string.in.table <- grep('.', all.tokens[[which.header.row]], value=TRUE)

	if ( !all(sort(string.in.header) == sort(string.in.table,)) ) {
		cat(sapply(1:length(string.in.header), function(.x) sprintf(' %s -- %s\n', row.meta[.x+1], string.in.table[.x])))
		stop(cat('*  Meta data mismatch between Header Meta vs meta data in table ... adat line:', which.header.row,'\n\n'))
	}


	# ------------------------------------------------ #
	# check lengths of each row
	# look for trailing tabs & non-square data block
	# header row can be off by 1 so remove
	# ------------------------------------------------ #
	token.lengths <- sapply(all.tokens[-which.header.row], length)					# remove empty header row between feature data and col meta
	data.lengths <- token.lengths[ which.col.meta.start:length(token.lengths) ] # lengths of data block

	# check if entire data block is of same length 
	# if tabs are wrong it won't be
	if ( !all(data.lengths==data.lengths[1]) ) {
		cat('  Tabs problem:\n')
		cat('  Token lengths:\n')
		print(data.lengths)
		tab <- table(data.lengths)
		names(dimnames(tab)) <- "  Table of Lengths:"
		print(tab)
		stop("  Number of tokens inconsistent for feature data or col.meta data\n    Check for trailing/missing tabs in the main block of the Adat\n    One (or more) of the above is different from the rest.")
	}

	# ---------------------------------------- #
	# check header row separately
	# compare to first row of feature data
	# do they differ by more than 10?
	# tab error in header row (java issue)
	# ---------------------------------------- #
	if ( length(all.tokens[[ which.header.row+1 ]]) - length(all.tokens[[ which.header.row ]]) > 10 ) {
		cat('\n*  Tabs Problem in Header row (should be >1000 in length):\n')
		print(all.tokens[[ which.header.row ]])
		stop("  Length of the header row tabs is incorrect\n    Does not extend the length of the columns")
	}


	# --------------------- #
	# check for empty strings
	# in entire Col.Meta block
	# may remove one day
	# --------------------- #
	string.gaps <- !sapply(all.tokens[which.col.meta.rows], function(.x) 
								  all(grepl(".",.x[col.meta.shift:length(.x)])))

	if ( any(string.gaps) ) {
		cat('\nEmpty strings detected in the Col.Meta block:\n  Visually inspect the following rows in adat:', col.meta2[string.gaps],'\n  They may be missing in Spuriomers and/or HybControls\n  They may be missing in SOMAmers without a EntrezGeneSymbol (e.g. a family)\n\n')
	}

	return(ret.list)

}
#### ---- END FUNCTION ---- ####
