# --------------------
# Revision Info
# --------------------
# $Revision: 16129 $
# $Author: sfield $
# $LastChangedDate: 2015-03-05 13:55:45 -0700 (Thu, 05 Mar 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/write.adat.R $
#######################################
#       Function:		write.adat
#######################################
write.adat <- function(dat, file, parent=NULL) {
	
	dat <- writer.prep(dat=dat, file=file, parent=parent)
	atts <- attributes(dat)

	# remove FEATURE_EXTRACTION & Checksum (recalculate)
	header.keep <- setdiff(names(atts$Header.Meta),c('Checksum','FEATURE_EXTRACTION'))
	atts$Header.Meta <- atts$Header.Meta[ header.keep ]

	# ----------------------- #
	#    Header Meta			  #
	# ----------------------- #
	eol <- get.eol()
	HM <- atts$Header.Meta    # rename for convenience

	for ( i in seq(length(HM)) ) {
		sublist.names <- names(HM[[i]]) 
		cat(sprintf('^%s%s', names(HM)[i], eol), file=file, append=i!=1)
		sapply(sublist.names, function(h)
				 cat(sprintf('!%s\t%s', h, paste(HM[[i]][[h]], collapse='\t')), eol,
					  append=TRUE, file=file, sep='')
				 )
	}
  
	# ----------------------- #
	#      write Col Meta     #
	# ----------------------- #
	meta.names <- get.meta(dat)			# get meta data names for use below
	length.meta <- length(meta.names)

	sapply(names(atts$Col.Meta), function(col.meta.name) 
			 cat(sprintf('%s%s\t%s', paste(rep('\t', length.meta), collapse=''), 
							 col.meta.name, paste(atts$Col.Meta[[col.meta.name]], collapse='\t')),
				  eol, sep='', append=TRUE, file=file))


	# ----------------------------- #
	#     write out header row      #
	# ----------------------------- #
	if ( nrow(dat) != 0 ) {				# Skip rest if Adat is empty

		if ( length.meta < 1 )
			warning(sprintf('You are writing an Adat without any meta data.\n  This will likely cause this file (%s) to be unreadable using read.adat().\n  Please include at least one column of meta data or do nothing if this was intented.', file))

		n.tabs <- sum(is.apt(names(dat))) - 1
		tabs <- paste(rep('\t', n.tabs), sep='', collapse='')
		metanames <- paste(meta.names, collapse='\t')
		header.row <- paste(metanames, tabs, sep='\t\t')
		cat(header.row, append=TRUE, file=file, sep='', eol)

		df<- dat
		df$mysterycol <- rep('',nrow(df))	# add mystery column
		df <- df[ ,c(meta.names, 'mysterycol', get.aptamers(df)) ]

		# ---------------------------------------- #
		#     write meta & feature data to file    #
		# ---------------------------------------- #
		df[,get.aptamers(df)] <- apply(df[,get.aptamers(df)], 2, function(.x) sprintf('%0.1f',.x))

		write.table(x=df, file=file, sep='\t', quote=FALSE, na='', eol=eol,
						row.names=FALSE, col.names=FALSE, append=TRUE)
	}

	write.checksum(file)
	cat(sprintf('*  Adat file written to .............. %s\n\n', paste(dirname(file),basename(file),sep='/'))) 


}
#######------- END FUNCTION -------#######
