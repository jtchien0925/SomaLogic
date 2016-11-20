# --------------------
# Revision Info
# --------------------
# $Revision: 16557 $
# $Author: sfield $
# $LastChangedDate: 2015-06-05 10:50:20 -0600 (Fri, 05 Jun 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/apt2seqid.R $
#######################################
#			Function:	apt2seqid
#######################################
apt2seqid <- function(apt) {

	token <- parse.apt(apt)			# pre-process apt name
	#print(token)

	q <- seq.regex(token)

	if ( q[[1]] == -1 )
		stop(paste("Couldn't find seq id for apt:", apt))

	if ( length(q[[1]]) > 1 )
		stop(paste("Multiple seq id for apt:", apt))

	end.match <- attributes(q)$match.length
	# sub acts only first match only; gsub acts on all
	sub("\\.", "-", substr(token, start=q[[1]], stop=q[[1]]+end.match-1))

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2013-10-18 17:12:17

