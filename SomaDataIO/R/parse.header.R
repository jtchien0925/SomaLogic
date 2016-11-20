# --------------------
# Revision Info
# --------------------
# $Revision: 16238 $
# $Author: sfield $
# $LastChangedDate: 2015-03-23 17:48:25 -0600 (Mon, 23 Mar 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/parse.header.R $
####################################
#			Function:	parse.header
####################################
parse.header <- function(adat) {

	line <- 0
	out.names <- c("Header.Meta","Col.Meta","file.specs")
	ret <- lapply(out.names, function(.x) list()) %names% out.names
	con <- file(adat, open="r")
	on.exit(close(con))

	repeat {
		row.data <- readLines(con, n=1, warn=FALSE)

		# if end of file reached before feature data = empty adat
		if ( ret$file.specs$EmptyAdat <- length(row.data)==0 ) 
			break

		line <- line + 1

		if ( grepl("Checksum", row.data) ) {
			ret$Header.Meta$Checksum <- strsplit(row.data, "\t")[[1]][2]
			next
		} else if ( grepl("^\\^.*[\t]{500,}$", row.data) ) { # catch for runaway tabs
			print(row.data)
			stop("\n  This does not appear to be a valid ADAT\n  One possibility is that there are empty tabs filling out the entire header block (see above)")
		}
		else if ( row.data=="^HEADER" ) {
			section<-"HEADER"
			next
		} else if ( row.data=="^COL_DATA" ) { 
			section<-"COL_DATA"
			next
		} else if ( row.data=="^ROW_DATA" ) { 
			section<-"ROW_DATA"
			next
		} else if ( row.data=="^TABLE_BEGIN" ) { 
			section <- "Col.Meta"
			ret$file.specs$table.begin <- line
			ret$file.specs$col.meta.start <- line + 1
			next
		} else if ( grepl("^\\^[A-Za-z]",row.data) ) { 
			section <- "Free.Form"
			free.field <- strsplit(row.data, "\t")[[1]][1]
			free.field <- substr(free.field, 2, nchar(free.field))
			next
		}
		
		tokens <- strsplit(row.data, "\t")[[1]]

		if ( is.na(tokens[1]) ) {
			cat('*  Warning: ADAT contains blank row(s) in Header section ... they will be skipped\n')
			next
		}

		first.empty <- tokens[1]==""			# is 1st entry empty string (col.meta section)
		alpha <- grep(".", tokens)				# get alpha numeric entries
		beta <- range(alpha)						# get range for valid entries; no empty strings
		tokens <- tokens[ beta[1]:beta[2] ]
		tokens[1] <- gsub("^[^A-Za-z]","",tokens[1])	# zap (!) and whitespace, non-alphanum, double dots etc.

		if ( section=="HEADER" ) {
			cur.header <- tokens[1]
			ret$Header.Meta[[section]][[cur.header]] <- list()
			ret$Header.Meta[[section]][[cur.header]] <- tokens[-1]
		}
		else if ( section=="COL_DATA" ) {
			ret$Header.Meta[[section]][[tokens[1]]] <- tokens[-1]
		}
		else if ( section=="ROW_DATA" ) {
			ret$Header.Meta[[section]][[tokens[1]]] <- tokens[-1]
		}
		else if ( section=="Free.Form" ) {
			cur.header <- tokens[1]
			ret$Header.Meta[[free.field]][[cur.header]] <- list()
			ret$Header.Meta[[free.field]][[cur.header]] <- tokens[-1]
		}
		else if ( section=="Col.Meta" ) {
			if ( first.empty ) {
				ret[[section]][[tokens[1]]] <- tokens[-1]
				ret$file.specs$col.meta.shift <- alpha[1]
			} else {						# if at end of col.meta section, break loop & stop reading file
				ret$file.specs$data.begin <- line
				ret$Header.Meta$TABLE_BEGIN = ""		# append empty token/marker to header & break
				break
			}
		}
	}
   
	ret$file.specs$old.adat <- version.check(ret$Header.Meta)		# TRUE if old adat version
	ret

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2015-03-06 20:56:07
