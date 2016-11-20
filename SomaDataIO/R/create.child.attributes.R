# --------------------
# Revision Info
# --------------------
# $Revision: 16374 $
# $Author: sfield $
# $LastChangedDate: 2015-04-17 09:50:21 -0600 (Fri, 17 Apr 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/create.child.attributes.R $
#############################################
#    Function:		create.child.attributes
#############################################
create.child.attributes <- function(child.df, parent.atts, verbose=TRUE) {

	child.apts <- get.aptamers(child.df)
	child.atts <- attributes(child.df)

	if ( is.intact.attributes(child.df) ) {
		cat('*  Child attributes intact ... TRUE\n')
		return(child.atts[ c('class','names','row.names','Header.Meta','Col.Meta') ])
	} else {
		cat('*  Broken child attributes ... reconstructing from parent\n')
	}


	# ----------------- #
	# fix header meta
	# ----------------- #
	meta.names <- get.meta(child.df)
	rowTypes <- sapply(meta.names, function(name) {
							 if ( is.Integer(child.df[,name], verbose=FALSE) )
								 'Integer'
							 else if ( is.numeric(child.df[,name]) )
								 'Float'
							 else  
								 'String' 
						})

	child.atts$Header.Meta <- parent.atts$Header.Meta
	child.atts$Header.Meta$ROW_DATA$Name <- meta.names			# ensures atts match data frame
	child.atts$Header.Meta$ROW_DATA$Type <- rowTypes
	
	
	
	
	# ----------------- #
	# fix col meta
	# ----------------- #
	parent.Col.Meta <- as.data.frame(parent.atts$Col.Meta, stringsAsFactors=FALSE) 
	parent.apts.n <- nrow(parent.Col.Meta)		# original no. features in parent

	if ( nrow(parent.Col.Meta) < 1 )
		stop("Incorrect dimensions parent Col.Meta ... are you passing parent ATTRIBUTES?") 

	# IF CALLING ON OLD ADAT VERSION
	if ( all(c("GeneName","Med.Scale.Hyb.Mix") %in% names(parent.Col.Meta)) )
		parent.rownames <- gsub("[^A-Za-z0-9]",".", paste(parent.Col.Meta$GeneName,
																		  parent.Col.Meta$SeqId))
	else
		parent.rownames <- gsub("[^A-Za-z0-9]",".", paste(parent.Col.Meta$EntrezGeneSymbol,
																		  parent.Col.Meta$SeqId)) 
	parent.rownames <- clean.names(parent.rownames)
	row.names(parent.Col.Meta) <- parent.rownames
	match.mat <- get.seq.ids.matches(parent.rownames, child.apts)
	#print(dim(match.mat))

	if ( nrow(match.mat) < length(child.apts) )
		stop("SOMAmers in child NOT in parent ... cannot create attributes from this parent.") 

	parent.Col.Meta <- parent.Col.Meta[ match.mat[,1], ]			# subset common apts parent:child
	child.apts <- child.apts[ child.apts %in% match.mat[,2] ]	# subset common apts child:parent
	row.names(parent.Col.Meta) <- child.apts							# reassign rownames of parent with child apt rownames for indexing


	#child.atts$Col.Meta <- as.list(parent.Col.Meta[ child.apts, ])	# shouldn't have to reorder: bc of reassignment above
	child.atts$Col.Meta <- as.list(parent.Col.Meta)
	col.meta.names <- names(child.atts$Col.Meta)
	colTypes <- sapply(col.meta.names, function(name) {
							 if ( is.integer(child.atts$Col.Meta[[name]]) )
								 "Integer"
							 else if ( is.numeric(child.atts$Col.Meta[[name]]) )
								 "Float"
							 else  
								 "String" 
						})

	child.atts$Header.Meta$COL_DATA$Name <- col.meta.names   # ensures header meta match col meta
	child.atts$Header.Meta$COL_DATA$Type <- colTypes




	if ( verbose ) {
		X <- parent.atts$Header.Meta$ROW_DATA
		cat(sprintf('\n*  Feature(s) removed .............. %i\n', parent.apts.n - length(child.apts)))
		cat(sprintf('*  Meta data field(s) removed ...... %i\n', length(setdiff(X$Name, get.meta(child.df)))))
		sapply(setdiff(X$Name, get.meta(child.df)), function(r) cat(sprintf('    Removed: %s\n', r)))

		new.meta <- setdiff(get.meta(child.df), X$Name)
		cat(sprintf('*  New meta data field(s) added .... %i\n', length(new.meta)))
		sapply(new.meta, function(n) cat(sprintf('    Added: %s\n', n)))

		cat(sprintf('*  Sample(s) removed ............... %i\n', length(parent.atts$row.names) - length(child.atts$row.names) ))
		#samples.rm <- setdiff(parent.atts$row.names, child.atts$row.names)

		cat('\nInternal Child Attributes Check:\n')
		logic.colmeta <- identical(child.atts$Header.Meta$COL_DATA$Name, names(child.atts$Col.Meta))
		cat(sprintf('*  Header.Meta COL_DATA == child col meta data fields ... %s\n', logic.colmeta))

		if ( !logic.colmeta ) {
			cat('*  They differ in:\n')
			tmp.diff <- vec.diff(child.atts$Header.Meta$COL_DATA$Name, names(child.atts$Col.Meta), verbose=FALSE)
			cat(sprintf('*    In COL_DATA not Col.Meta:  %s\n', tmp.diff$first.only))
			cat(sprintf('*    In Col.Meta not COL_DATA:  %s\n\n', tmp.diff$second.only))
		}

		logic.rowmeta <- identical(child.atts$Header.Meta$ROW_DATA$Name, get.meta(child.df))
		cat(sprintf('*  Header.Meta ROW_DATA == child row meta data fields ... %s\n', logic.rowmeta))

		if ( !logic.rowmeta ) {
			cat('*  They differ in:\n')
			tmp.diff <- vec.diff(child.atts$Header.Meta$ROW_DATA$Name, get.meta(child.df))
			cat(sprintf("*    In ROW_DATA not MetaData:  %s\n", tmp.diff$first.only))
			cat(sprintf("*    In MetaData not ROW_DATA:  %s\n\n", tmp.diff$second.only))
		}

	}

	child.atts[ c("class","names","row.names","Header.Meta","Col.Meta") ]    # consistent order

}

#### ---- END FUNCTION ---- ####

# ---- created on: 2013-12-18 09:43:16

