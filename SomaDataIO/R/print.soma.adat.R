# --------------------
# Revision Info
# --------------------
# $Revision: 16453 $
# $Author: sfield $
# $LastChangedDate: 2015-05-04 09:52:22 -0600 (Mon, 04 May 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/print.soma.adat.R $
#########################################
#        Function: print.soma.adat
#########################################
print.soma.adat <- function(x, ...) {

	if ( ncol(x) <= 50 || nrow(x) <= 2 )	# if small, just use default data.frame print method
		return(print.data.frame(x))

	cat(sprintf("* Attributes intact ...... %s\n", is.intact.attributes(x)))

	cat("* Dimensions:\n")
	cat(sprintf("    Rows ................. %i\n", length(row.names(x))))
	cat(sprintf("    Columns .............. %i\n", length(names(x))))
	cat(sprintf("    MetaData ............. %i\n", length(get.meta(x))))
	cat(sprintf("    FeatureData .......... %i\n", length(get.aptamers(x))))
	cat(sprintf("    Response Present ..... %s\n", "Response" %in% names(x)))
	cat("* Header Data:\n")

	if ( !"Header.Meta" %in% names(attributes(x)) )
		cat("    No Header Data: adat was probably modified, which strips the attributes ...\n") 
	else {
		A <- attributes(x)$Header.Meta
		calc.max <- function(x) max(sapply(x, function(i) nchar(i)))
		max <- calc.max(names(A$HEADER))
		my.sapply(1:length(A$HEADER), function(x) {
					 n <- nchar(names(A$HEADER[x]))
					 dots <- paste(rep(".",max+3-n),collapse="")
					 cat(sprintf("    %s %s %s\n", names(A$HEADER[x]), dots, A$HEADER[[x]]))})
		cat("* Column Meta:\n")
		B <- attributes(x)$Col.Meta
		max <- calc.max(names(B))
		my.sapply(1:length(B), function(x) {
					 n <- nchar(names(B[x]))
					 dots <- paste(rep(".",max+3-n),collapse="")
					 cat(sprintf("    %s %s %s (%i)\n", names(B[x]), dots, class(B[[x]]), length(B[[x]])))})
	}

}



summary.soma.adat <- function(object, soma.data=NULL, digits=max(3L, getOption("digits") - 3L), ...) {

	if ( !is.null(soma.data) )
		ad <- soma.data
	else if ( is.intact.attributes(object) )
		ad <- get.apt.data(object)
	else if ( any(c("soma.data","apt.data") %in% ls(.GlobalEnv)) ) {
		tabl <- intersect(c("soma.data","apt.data"), ls(.GlobalEnv))[1]
		ad <- get(tabl,.GlobalEnv)
	}

	nm <- get.aptamers(object)
	nc <- length(nm)
	new.labels <- c("Min","1Q","Median","Mean","3Q","Max","sd","MAD","IQR")		# rename labels
	nr <- length(new.labels) + 1
	col.nchar <- numeric(nc)

	summ <- lapply(nm, function(.x) {
						vec <- object[[.x]]
						vec <- vec[!is.na(vec)] # rm NaN/NA
						fn <- c(summary, sd, mad, IQR)
						unlist(sapply(fn, function(f) f(vec,...))) %names% new.labels
				}) %names% nm

	z = list()

	for ( i in seq_len(nc) ) {
		nums <- format(summ[[i]], digits=digits)
		if ( "ad"%in%ls() && nm[i] %in% rn(ad) )
			tgts <- ad[ nm[i], "Target" ]
		else
			tgts <- ""
		vals <- c(Target=tgts,nums)
		labs <- format(names(vals))
		col.nchar[i] <- nchar(labs[1L], type="w")
		z[[i]] <- paste0(labs, ":", vals, "  ")
	}

	z <- unlist(z, use.names=TRUE)
	dim(z) <- c(nr,nc)
	blanks <- paste(character(max(nchar(nm, type="w"), na.rm=TRUE)+2L), collapse=" ")
	pad <- floor(col.nchar - nchar(nm, type="w") / 2)			# pad blanks if short name
	final.nm <- paste0(substring(blanks, 1, pad), nm)
	dimnames(z) <- list(rep.int("",nr),final.nm)
	class(z) <- "table"
	z
}



