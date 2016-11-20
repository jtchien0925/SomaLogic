# --------------------
# Revision Info
# --------------------
# $Revision: 16549 $
# $Author: sfield $
# $LastChangedDate: 2015-05-29 13:56:16 -0600 (Fri, 29 May 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/read.adat.R $
#######################################
#			Function:	read.adat
#			Function:	load.adat
#			Function:	load.legacy
#######################################
read.adat <- load.adat <- function(file, protein.label="EntrezGeneSymbol", remove.buffer=TRUE,
											  replace.names=TRUE, verbose=getOption("verbose"), debug=FALSE, ...) {

	stopifnot( file.exists(file) )

	if ( debug )
		return(parse.check(strsplit(readLines(file, n=200),"\t"), verbose=TRUE))

	if ( !verify.adat.checksum(file) )
		warning('*  Unable to verify (or find) the Adat Checksum ... consider the following:\n   1) There is no Checksum line in the ADAT (see note above to confirm)\n   2) This file may have been modified and the Checksum not updated\n', call.=FALSE)

	header.data <- parse.header(file)

	if ( header.data$file.specs$EmptyAdat ) {
		warning("No feature data in ADAT ... returning data frame of Column Meta data", call.=FALSE)
		pdat <- data.frame(header.data$Col.Meta, stringsAsFactors=FALSE)
		attributes(pdat) <- c(attributes(pdat), header.data$Header.Meta)
		return(pdat)
	}
	
	skip <- header.data$file.specs$data.begin - 1
	pdat <- load.pdat(file, skip=skip, ...)

	prot.vec <- unique(c(protein.label,"EntrezGeneSymbol","GeneName","Target","SomaId"))

	if ( !protein.label %in% names(header.data$Col.Meta) ) {
		for ( i in prot.vec ) {
			if ( i %in% names(header.data$Col.Meta) ) {
				warning(sprintf("The supplied argument [protein.label=%s] was not available in the Col.Meta\n  The following field was used instead ... %s", protein.label, i))
				break
			}
		}
	}


	if ( all(!prot.vec %in% names(header.data$Col.Meta)) ) {		# if none present in Col.Meta
		stop(sprintf("  Protein label(s) [%s] missing from SOMAmer column data", paste(prot.vec,collapse=", ")), call.=FALSE)
	}
	else {
		for ( labs in prot.vec ) {
			if ( labs %in% names(header.data$Col.Meta) ) {
				target.names <- header.data$Col.Meta[[ labs ]]
				break
			}
		}
	}

	target.names <- clean.names(target.names)		# zap trailing spaces, double/leading dots, non-alphanum
	#print(target.names)


	if ( replace.names && "Type" %in% names(header.data$Col.Meta) ) {
		replace.empty <- "NoneX"
		target.names <- fix.target.names(target.names, header.data$Col.Meta$Type, replace.empty)
	}

	if ( "Header.Meta" %in% names(header.data) ) {
		if ( "ROW_DATA"  %in% names(header.data$Header.Meta) ) {
			if ( !"Name" %in% names(header.data$Header.Meta$ROW_DATA) )
				stop("Couldn't find Name in ROW_DATA in Header.Meta", call.=FALSE)
		}
		else
			stop("ROW_DATA not in Header.Meta", call.=FALSE)
	}
	else
		stop("Couldn't find Header.Meta", call.=FALSE)

	row.meta.head <- header.data$Header.Meta$ROW_DATA$Name

	if ( any(duplicated(row.meta.head)) )
		stop("Duplicate row meta data fields defined in header ROW_DATA", call.=FALSE)

	row.meta.len <- length(row.meta.head)			# define how many meta names to grab
	row.meta <- head(names(pdat), row.meta.len)	# get names from pdat
  	row.meta <- clean.names(row.meta)				# zap trailing spaces, double/leading dots, non-alphanum

	old.adat <- header.data$file.specs$old.adat
	if ( !old.adat )
		row.meta <- c(row.meta, "")		# Account for empty column in version 1.0 or greater: sgf
		
	
	if ( length(header.data$Col.Meta) > 0 ) {

		if ( "SeqId" %in% names(header.data$Col.Meta) )
			apts <- header.data$Col.Meta$SeqId
		else if ( "SomaId" %in% names(header.data$Col.Meta) )
			apts <- header.data$Col.Meta$SomaId
		else  {
			cat('*  Old adat format. Assuming SeqId is in the mystery row\n')
			lines <- scan(file, what="char", sep="\n", quiet=TRUE)[[skip+1]]
			#print(lines)
			apts <- strsplit(lines, '\t')[[1]][ (length(row.meta)+1):ncol(pdat) ]	# Assuming SeqId is mystery row
			#print(ht(apts))
		}

		new.names <- paste(target.names, apts, sep=".")
		new.names <- clean.names(new.names)			# in case trailing or leading '.' crept in
		#print(ht(new.names))
	}
	else
		stop("No Col.Meta data found in adat", call.=FALSE)

	pdim <- length(row.meta) + length(new.names)

	if ( ncol(pdat) > pdim )
		warning('Num. of columns in "pdat" greater than meta + aptamers length\n  Possible trailing tabs OR the old/new adat version is incorrect\n  This could SERIOUSLY affect your data\n  Please set verbose=TRUE and recall load.adat()')
	else if ( pdim - ncol(pdat) == 1 )
		stop("Num. of columns in 'pdat' less than (meta + aptamers) by 1\n  The likely reason is that the old/new adat version doesn't match ... check '1.0' vs. '1.0.0' in the Adat and recall with 'verbose=TRUE'\n", call.=FALSE)
	else if ("SeqId" %in% names(header.data$Col.Meta)) {
		if ( length(header.data$Col.Meta$SeqId) != get.aptamers(header.data$Col.Meta$SeqId, n=TRUE) ) {
			cat("Mis-formatted SeqIds:", setdiff(header.data$Col.Meta$SeqId, get.aptamers(header.data$Col.Meta$SeqId)),"\n")
			stop("Some SeqIds in Col.Meta block are not proper SeqIds (see above)", call.=FALSE)
		}
	}

	# fix for trailing tabs in pdat table
	pdat <- pdat[ , 1:pdim ] %names% c(row.meta, new.names)

	if ( remove.buffer ) {
 		if ( "SampleType" %in% names(pdat) )
			pdat <- pdat[ !grepl("Buffer", pdat$SampleType, ignore.case=TRUE), ]
 		if ("ClassName" %in% names(pdat))
			pdat <- pdat[ !grepl("Buffer", pdat$ClassName, ignore.case=TRUE), ]
		pdat <- pdat[ !grepl("Buffer", pdat$SampleId, ignore.case=TRUE), ]
	}

	# remove ghost column if NOT old adat
	if ( "" %in% names(pdat) )
		pdat <- pdat[ , -which(names(pdat) == "") ]
	
	if ( verbose ) {
		if ( empty.logic )
			cat(sprintf('\n*  Empty string detected in chosen protein label argument ... replaced with: %s\n',replace.empty))
		cat(sprintf('*  Skip caculated as ......... %i\n', skip))
		cat(sprintf('*  Adat version .............. %s\n', header.data$Header.Meta$HEADER$Version))
		cat(sprintf('*  Table Begin ............... %s\n', header.data$file.specs$table.begin))
		cat(sprintf('*  Col.Meta Start ............ %s\n', header.data$file.specs$col.meta.start))
		cat(sprintf('*  Col.Meta Shift ............ %s\n', header.data$file.specs$col.meta.shift))
		cat(sprintf('*  Header Row ................ %s\n', header.data$file.specs$data.begin))
		cat(sprintf('*  Old Adat version .......... %s\n', old.adat))		# T/F old.adat; call occurs in parse.header()
		cat(sprintf('*  Length row meta ........... %i\n', length(row.meta)))
		cat(sprintf('*  Length targets (apts) ..... %i\n', length(new.names)))
		cat(sprintf('*  Dim pdat .................. %s\n', paste(dim(pdat), collapse=' x ')))
		tmp <- as.data.frame(header.data$Col.Meta, stringsAsFactors=FALSE)
		cat(sprintf('*  Dim Col Meta .............. %s\n', paste(dim(tmp), collapse=' x ')))
		cat('\n*  Head Col Meta:\n')
		print(head(tmp))
		cat('\n*  Head/Tail Feature Data (final 3 cols):\n')
		print(ht(pdat[,c((ncol(pdat)-2):ncol(pdat))]))
	}

	attributes(pdat) <- c(attributes(pdat), header.data)
	attributes(pdat)$apt.data <- get.apt.data(pdat)
  	class(pdat) <- c( "soma.adat", class(pdat) )
	return(pdat)

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2015-03-07 12:15:52












################################### 
#	Function:	load.legacy
#	This function is old legacy code written to load out-of-spec adats
#	It may work on older ADATs if the current load.adat() fails
#	It will not be maintained and will eventually become deprecated
################################### 
load.legacy <- function(file, protein.label="EntrezGeneSymbol", remove.buffer=TRUE,
								replace.names=TRUE, verbose=getOption("verbose"), debug=FALSE) {

	stopifnot( file.exists(file) )

	if ( debug )
		return(parse.check(strsplit(readLines(file,n=200),"\t"),verbose=TRUE))

	if ( !verify.adat.checksum(file) )
		warning('*  Unable to verify (or find) the Adat Checksum ... consider the following:\n  1) There is no Checksum line in the Adat\n  2) This file may have been modified and the Checksum was not updated\n')

	header.data <- parse.adat.header(file, verbose=verbose)
	old.adat <- attributes(header.data)$old.adat

	if ( attributes(header.data)$EmptyAdat ) {
		warning('No feature data in Adat file ... returning empty data frame with attributes')
		pdat <- data.frame(header.data$Col.Meta, stringsAsFactors=FALSE)
		attributes(pdat) <- c(attributes(pdat), header.data$Header.Meta)
		return(pdat)
	}
	
	skip <- attributes(header.data)$HeaderRow - 1
	pdat <- load.pdat(file, skip=skip)


	prot.vec <- unique(c(protein.label,'EntrezGeneSymbol','GeneName','Target','SomaId'))

	if ( !protein.label %in% names(header.data$Col.Meta) ) {
		for ( i in prot.vec ) {
			if ( i %in% names(header.data$Col.Meta) ) {
				warning(sprintf('The supplied argument protein.label="%s" was not available in the Col.Meta\n  The following field was used instead ... %s', protein.label, i))
				break
			}
		}
	}


	if ( all(!prot.vec %in% names(header.data$Col.Meta)) ) {		# if none present in Col.Meta
		stop(sprintf('  Protein label "%s" missing from SOMAmer column data', prot.vec))
	}
	else {
		for ( labs in prot.vec ) {
			if ( labs %in% names(header.data$Col.Meta) ) {
				target.names <- header.data$Col.Meta[[ labs ]]
				break
			}
		}
	}

	target.names <- clean.names(target.names)		# zap trailing spaces, double/leading dots, non-alphanum

	empty <- grep(".", target.names, invert=TRUE)
	if ( length(empty) > 0 ) {
		target.names[empty] <- "NoneX"				# fix for empty strings in Col.Meta
		if ( verbose )
			cat('*  Empty string detected in chosen protein label argument ... replaced with "NoneX"\n')
	}
	
	#print(target.names)

	if ( replace.names ) {
		if ( 'Type' %in% names(header.data$Col.Meta) ) {
			types <- header.data$Col.Meta$Type

			indices <- grep('Spuriomer', types)
			target.names[indices] <- 'Spuriomer'
			
			indices <- grep('ybrid.*Block$', types, ignore.case=TRUE)
			target.names[indices] <- 'HybControlBlock'

			indices <- grep('ybrid.*Elution$', types, ignore.case=TRUE)
			target.names[indices] <- 'HybControlElution'

			indices <- grep('^Non[\\.-]human$', types, ignore.case=TRUE)
			target.names[indices] <- 'NonHuman'
			target.names <- gsub('^non[\\.-]human$', 'NonHuman', target.names, ignore.case=TRUE)
			
			indices <- grep('^Non-Cleavable', types, ignore.case=TRUE)
			target.names[indices] <- 'NonCleavable'

			indices <- grep('Biotin$', types, ignore.case=TRUE)
			target.names[indices] <- 'NonBiotin'

			rm(indices, types) # cleanup
		}
	}

	if ( 'Header.Meta' %in% names(header.data) ) {
		if ( 'ROW_DATA'  %in% names(header.data$Header.Meta) ) {
			if ( !'Name' %in% names(header.data$Header.Meta$ROW_DATA) )
				stop("Couldn't find Name in ROW_DATA in Header.Meta")
		}
		else
			stop("ROW_DATA not in Header.Meta")
	}
	else
		stop("Couldn't find Header.Meta")


  	defined.row.meta <- grep('.', header.data$Header.Meta$ROW_DATA$Name, value=TRUE)		# double caution; zaps empty tabs
	dupes1 <- any(duplicated(defined.row.meta))
	# Same operation to the defined meta data before the check
  	defined.row.meta <- clean.names(defined.row.meta)		# zap non-alphanum
	dupes2 <- any(duplicated(defined.row.meta))

	if ( dupes1 )
		stop("Duplicate row names defined in header")
	if ( dupes2 )
		stop("Duplicate row names defined in header after R gsub")

	row.meta.len <- length(defined.row.meta)				# define how many meta names to grab
	row.meta <- names(pdat)[ 1:row.meta.len ]				# get from pdat
  	row.meta <- clean.names(row.meta)						# zap non-alphanumerics

	if ( !old.adat )
		row.meta <- c(row.meta, '')		# Account for empty column in version 1.0 or greater: sgf
		
	new.names = NULL			# why is this here? sgf 21/11/2014
	
	if ( length(header.data$Col.Meta) > 0 ) {

		if ( 'SeqId' %in% names(header.data$Col.Meta) )
			apts <- header.data$Col.Meta$SeqId
		else if ( 'SomaId' %in% names(header.data$Col.Meta) )
			apts <- header.data$Col.Meta$SomaId
		else  {
			cat('*  Old adat format. Assuming SeqId is in the mystery row\n')
			lines <- scan(file, what='char', sep='\n', quiet=TRUE)[[skip+1]]
			#print(lines)
			#apts <- strsplit(lines, '\t')[[1]][ length(row.meta):ncol(pdat) ]		# Assuming SeqId is mystery row; evtl. rm sgf
			apts <- strsplit(lines, '\t')[[1]][ (length(row.meta)+1):ncol(pdat) ]	# Assuming SeqId is mystery row
			#print(ht(apts))
		}

		if ( is.null(target.names) )
			new.names <- apts
		else
			new.names <- paste(target.names, apts)
		
		new.names <- gsub('[^A-Za-z0-9]', '.', new.names)
		new.names <- gsub('\\.{2,3}', '.', new.names)			# zap double or triple dots in name
		#print(ht(new.names))
	}
	else
		warning('*  No Col.Meta data found in adat!')

	pdim <- length(row.meta) + length(new.names)

	if ( verbose ) {
		# T/F old.adat; call occurs in parse.adat()
		cat(sprintf('*  Skip caculated as ......... %i\n', skip))
		cat(sprintf('*  Adat version .............. %s\n', header.data$Header.Meta$HEADER$Version))
		cat(sprintf('*  Old Adat version .......... %s\n', version.check(header.data$Header.Meta)))
		cat(sprintf('*  Length row meta ........... %i\n', length(row.meta)))
		cat(sprintf('*  Length new names (apts) ... %i\n', length(new.names)))
		cat(sprintf('*  Dim pdat .................. %s\n', paste(dim(pdat), collapse=' x ')))
		tmp <- as.data.frame(header.data$Col.Meta, stringsAsFactors=FALSE)
		cat(sprintf('*  Dim Col Meta .............. %s\n', paste(dim(tmp), collapse=' x ')))
		print(head(tmp))
		print(ht(pdat[,c(ncol(pdat)-1,ncol(pdat))]))
	}

	if ( ncol(pdat) > pdim )
		warning('Num. of columns in "pdat" greater than meta + aptamers length\n  Possible trailing tabs OR the old/new adat version is incorrect\n  This could SERIOUSLY affect your data\n  Please set verbose=TRUE and recall load.adat()')
	else if ( pdim - ncol(pdat) == 1 )
		stop("Num. of columns in 'pdat' less than (meta + aptamers) by 1\n  The likely reason is that the old/new adat version doesn't match ... check '1.0' vs. '1.0.0' in the Adat and recall with 'verbose=TRUE'\n")
	else if ("SeqId" %in% names(header.data$Col.Meta)) {
		if ( length(header.data$Col.Meta$SeqId) != get.aptamers(header.data$Col.Meta$SeqId, n=TRUE) )
			cat("Mis-formatted SeqIds:", setdiff(header.data$Col.Meta$SeqId, get.aptamers(header.data$Col.Meta$SeqId)),'\n')
			stop("Some SeqIds in Col.Meta block are not proper SeqIds (see above)", call.=FALSE)
	}

	# fix for trailing tabs in pdat table
	pdat <- pdat[ , 1:pdim ]
	names(pdat) <- c(row.meta, new.names)


	if ( remove.buffer ) {
 		if ( 'SampleType' %in% names(pdat) )
			pdat <- pdat[ !grepl('Buffer', pdat$SampleType, ignore.case=TRUE), ]
 		if ('ClassName' %in% names(pdat))
			pdat <- pdat[ !grepl('Buffer', pdat$ClassName, ignore.case=TRUE), ]
		pdat <- pdat[ !grepl('Buffer', pdat$SampleId, ignore.case=TRUE), ]
	}

	# remove ghost column if NOT old adat
	if ( '' %in% names(pdat) )
		pdat <- pdat[ , -which(names(pdat) == '') ]
	
	attributes(pdat) <- c(attributes(pdat), header.data)
  	class(pdat) <- c( "soma.adat", class(pdat) )
	return(pdat)

}
#### ---- END FUNCTION ---- ####

