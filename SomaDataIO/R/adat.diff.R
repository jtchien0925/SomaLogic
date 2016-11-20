# --------------------
# Revision Info
# --------------------
# $Revision: 16487 $
# $Author: sfield $
# $LastChangedDate: 2015-05-19 17:40:07 -0600 (Tue, 19 May 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/adat.diff.R $
#######################################
#			Function:	adat.diff
#######################################
adat.diff <- function(adat1, adat2, tolerance=1e-08) {

	if ( !is.class(adat1,"data.frame") || !is.class(adat2,"data.frame") )
		stop("Either adat1 or adat2 is not a data.frame object ... please check class(adat)")

	cat("*  Checking characteristics:\n")
	cat("*  Attributes names are identical .......", identical(names(attributes(adat1)), names(attributes(adat2))), "\n")
	cat("*  Attributes are identical .............", identical(attributes(adat1), attributes(adat2)),"\n")
	cat("*  Adat dimensions are identical ........", all(dim(adat1)==dim(adat2)),"\n")

	if ( !all(dim(adat1)==dim(adat2)) ) {
		cat("*  ADATs have same no. of rows ..........", nrow(adat1)==nrow(adat2),"\n")
		cat("*  ADATs have same no. of columns .......", ncol(adat1)==ncol(adat2),"\n")
		cat("*  ADATs have same no. of features ......", get.aptamers(adat1,n=TRUE) == get.aptamers(adat2,n=TRUE),"\n")
		cat("*  ADATs have same no. of meta fields ...", get.meta(adat1,TRUE) == get.meta(adat2,TRUE) ,"\n")
	}

	cat("*  Adat row names are identical .........", isTRUE(all.equal(rn(adat1), rn(adat2))),"\n")
	same.ft.names <- isTRUE(all.equal(get.aptamers(adat1), get.aptamers(adat2)))
	same.meta.names <- isTRUE(all.equal(get.meta(adat1), get.meta(adat2)))
	cat("*  Adats contain same Features ..........", same.ft.names,"\n")
	cat("*  Adats contain same Meta Fields .......", same.meta.names,"\n")

	if ( !same.meta.names || !same.ft.names ) {
		apts1.2 <- setdiff(get.aptamers(adat1), get.aptamers(adat2))
		apts2.1 <- setdiff(get.aptamers(adat2), get.aptamers(adat1))
		meta1.2 <- setdiff(get.meta(adat1), get.meta(adat2))
		meta2.1 <- setdiff(get.meta(adat2), get.meta(adat1))

		if ( length(apts1.2) > 0 ) {
			cat(sprintf("The following features in [%s] but not [%s]:\n",deparse(substitute(adat1)),deparse(substitute(adat2))))
			sapply(apts1.2, function(n) cat(sprintf('   %s\n',n)))
		}
		if ( length(apts2.1) > 0 ) {
			cat(sprintf("The following features in [%s] but not [%s]:\n",deparse(substitute(adat2)),deparse(substitute(adat1))))
			sapply(apts2.1, function(i) cat(sprintf("   %s\n",i)))
		}
		if ( length(meta1.2) > 0 ) {
			cat(sprintf("The following meta data in [%s] but not [%s]:\n",deparse(substitute(adat1)),deparse(substitute(adat2))))
			sapply(meta1.2, function(i) cat(sprintf("   %s\n",i)))
		}
		if ( length(meta2.1) > 0 ) {
			cat(sprintf("The following meta data in [%s] but not [%s]:\n",deparse(substitute(adat2)),deparse(substitute(adat1))))
			sapply(meta2.1, function(i) cat(sprintf("   %s\n",i)))
		}
		cat("\n*  Diff check continuing on the **INTERSECT** of ADAT names (columns)\n")
	}

	# up to here, all but content/values identical; check now
	cat("*  Checking data content (values):\n")
	ret <- lapply(as.logical(1:0), function(l)
					  diff.check.cols(adat1, adat2, meta=l, tolerance=tolerance)) %names% c("meta_diffs","feature_diffs")

	if ( length(ret)>0 )
		ret
	else
		"Data table is identical"

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2013-06-19 10:44:27
