# --------------------
# Revision Info
# --------------------
# $Revision: 16429 $
# $Author: sfield $
# $LastChangedDate: 2015-04-23 13:48:41 -0600 (Thu, 23 Apr 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/checksum.R $
#################################

check.file <- function(file) {
	f <- path.expand(file)
	if ( !file.exists(f) )
		stop("\n  The file does not exist: ", f, call.=FALSE)
	else if ( !isTRUE(!file.info(f)$isdir) )
		stop("\n  The specified pathname does not point to a file: ", f, call.=FALSE)
	else
		TRUE
}


get.eol <- function() {
	os <- R.version$os
	if ( grepl("linux",os) )  # linux
		"\r\n"
	else if ( grepl("ming",os) )  # windows
		"\n"
	else if ( grepl("darwin",os) )  # mac
		"\n"
	else
		"\r\n"
}


get.checksum <- function(file) {
	check.file(file)
	line1 <- scan(file, sep="\t", what="char", nlines=1, quiet=TRUE)
	if ( grepl("Checksum", line1[1], ignore.case=TRUE) && length(line1) > 1 )
		line1[2]
	else {
		cat("*  No Checksum in line 1 of Adat\n")
		'NULL'
	}
}


calc.sha1sum <- function(file) { 
	file <- path.expand(file)
	algoint <- 102
	length <- -1
	skip <- seed <- raw <- 0
	.Call(digest:::digest_impl, file, algoint, length, skip, raw, seed)
}


calc.checksum <- function(file) {
	check.file(file)
	lines <- readLines(file)
	if ( grepl("Checksum", lines[1], ignore.case=TRUE) ) {
		tmpfile <- paste(paste(sample(c(letters[1:26],0:9), 10), collapse=""),"txt",sep=".")
		on.exit(file.remove(tmpfile))
		cat(sprintf("%s%s", lines[-1], get.eol()), file=tmpfile, sep="")
		calc.sha1sum(tmpfile)
	}
	else
		calc.sha1sum(file)
}


verify.adat.checksum <- function(file) {
	check.file(file)
	get.checksum(file) == calc.checksum(file)
}


write.checksum <- function(file) {
	check.file(file)
	lines <- readLines(file)
	if ( grepl("Checksum", lines[1], ignore.case=TRUE) ) {
		stop("\n  The file (", file, ") already has a Checksum in line 1", call.=FALSE)
	} else {
		chksum <- calc.sha1sum(file)
		chksum <- sprintf("!Checksum\t%s%s", chksum, get.eol())
		tmpfile <- paste(paste(sample(c(letters[1:26],0:9), 10), collapse=""),"txt",sep=".")
		cat(chksum, file=tmpfile)
		file.append(tmpfile,file)
		invisible(file.rename(tmpfile,file))
	}
}


