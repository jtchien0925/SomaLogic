# --------------------
# Revision Info
# --------------------
# $Revision: 15952 $
# $Author: sfield $
# $LastChangedDate: 2015-02-04 16:36:56 -0700 (Wed, 04 Feb 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/version.check.R $
#######################################
#			Function:	version.check
#######################################
version.check <- function(x) { 
	# checks if the adat is an 'old' version adat (without extra column)
	# TRUE: old version
	# FALSE: new version
	# x: Header.Meta
	#####################################################
	if (!'Version' %in% names(x[['HEADER']]) ) 
		stop("No Version in adat!") 

	version <- x[['HEADER']][['Version']]

	if ( length(version) > 1 ) {
		warning('Version length > 1 ... it appears there are empty tabs in the header block filling out the data matrix')
		version <- version[1]
	}

	if ( version == '1.01' )				# java version catch 1.01: sgf
		stop("Fix java adat writer! version cannot be 1.01 (set=1.0.1)")

	version < '1.0.0'		# check if version is '1.0' or less (TRUE if old adat)

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2013-10-15 18:54:07
