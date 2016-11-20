# --------------------
# Revision Info
# --------------------
# $Revision: 16523 $
# $Author: sfield $
# $LastChangedDate: 2015-05-21 10:45:18 -0600 (Thu, 21 May 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaDataIO/R/onAttach.R $
################################## 
#			Function:	.onAttach
#			Aliases attached when SomaGlobals is attached to Namespace
################################## 
.onAttach <- function(libname, pkgname) {
	packageStartupMessage("\n  Welcome to the SomaPackages(TM) software suite, developed by SomaLogic's bioinformatics team. SomaLogic Copyright 2015.")
	packageStartupMessage("\n  This software is licensed only to you (the \"Licensee\") as a customer of SomaLogic for use with data provided by SomaLogic and for no other purpose. SomaLogic licenses the licensed software \"as is\" and, by using this software, Licensee accepts the terms of this limited license. SomaLogic offers no warranties as to the function or use of the licensed software, whether express, implied, or statutory, including, without limitation, any implied warranties of merchantability or fitness for a particular purpose. The entire risk as to the quality and performance of the licensed software is with licensee. Licensor does not warrant that the functions contained in the licensed software will meet licensee's requirements or that the licensed software will be error-free.")
	.hidden <- new.env(parent=.GlobalEnv)
	.hidden$my.sapply <- function(x, ..., transpose=FALSE) {
		tmp <- sapply(x, ...)
		if ( is.character(x) && length(x)==length(tmp))
			names(tmp) <- x
		if ( transpose )
			tmp <- t(tmp)
		invisible(tmp)
	}
	.hidden$rn <- base::rownames
	.hidden$ht <- function(d) data.frame(rbind(head(d),tail(d)), stringsAsFactors=FALSE)
	attach(.hidden, warn.conflicts=FALSE)
}
