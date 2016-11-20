# --------------------
# Revision Info
# --------------------
# $Revision: 16162 $
# $Author: sfield $
# $LastChangedDate: 2015-03-10 10:20:30 -0600 (Tue, 10 Mar 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/parse.adat.header.R $
#######################################
#			Function:	parse.adat.header
#######################################
# This function will soon be deprecated and superceeded by parse.header(); sgf
parse.adat.header <- function(adat, ...) {

	lines.list <- strsplit(readLines(adat, n=200), "\t", fixed=TRUE)	# lines of the adat (encompass header)
	parse.info <- parse.check(lines.list, ...)
	header.data = list()
	col.meta = list()
	line.count <- 0

	for ( tokens in lines.list ) {
		line.count <- line.count + 1

		# if finished with all Col.Meta data; break loop
		if ( line.count == parse.info$HeaderRow )
			break

		if ( is.na(tokens[1]) ) {
			cat('*  Warning: adat file contains blank row(s) in Header section ... they will be skipped\n')
			next
		}

		# ----------------------- #
		#	First the Header.Meta
		# ----------------------- #
		if ( line.count < parse.info$ColMetaStart ) {

			if ( tokens[1] == '!Checksum' ) {
				header.data[['Checksum']] = tokens[2]
				next
			}
			
			if ( grepl('^\\^', tokens[1]) ) {								# Is Header section?
				cur.header.section <- gsub('^\\^','',tokens[1])			# get header string
				header.data[[cur.header.section]] = list()
			}
				

			if (  grepl('!|#', tokens[1]) ) {								# Is data field
				field <- gsub('!|#','',tokens[1])
				if ( length(tokens) == 1 )
					header.data[[cur.header.section]][[field]] = character(0)
				else
					header.data[[cur.header.section]][[field]] = tokens[-1]
			}
			
			if ( length(names(header.data))==3 && !'old.adat'%in%ls() ) {	# fix: one call for old adats
				# once we get this far, check the version in the header
				old.adat <- version.check(header.data)		# T/F
			}
		}

		# -------------------- #
		#	Now the Col.Meta
		# -------------------- #
		else {
			tokens <- tokens[ parse.info$ColMetaShift:length(tokens) ]		# cut out leading tabs
			col.meta.name <- tokens[1]
			#print(col.meta.name)
			col.meta.values <- tokens[2:length(tokens)]

			if ( all(is.seq(col.meta.values)) )		# catch for old adats with SeqId in mystery row
				col.meta.name <- 'SeqId'

			col.meta[[col.meta.name]] <- col.meta.values
		}

	}


  	names(col.meta) <- gsub('[^A-Za-z0-9]', '.', names(col.meta))			# zap non-alphanum names
	#cat("*  line.count =",line.count,'\n')
	header.data.out <- list()
	header.data.out$Header.Meta <- header.data
	header.data.out$Col.Meta <- make.numeric(col.meta)
	attributes(header.data.out)$old.adat <- old.adat
	attributes(header.data.out)$HeaderRow <- parse.info$HeaderRow
	no.data <- parse.info$HeaderRow >= length(lines.list)
	attributes(header.data.out)$EmptyAdat <- ifelse(no.data,TRUE,FALSE)

	return(header.data.out)

}
#### ---- END FUNCTION ---- ####
