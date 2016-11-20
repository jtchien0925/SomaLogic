# --------------------
# Revision Info
# --------------------
# $Revision: 16265 $
# $Author: sfield $
# $LastChangedDate: 2015-03-25 10:20:55 -0600 (Wed, 25 Mar 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/get.seq.ids.matches.R $
#######################################
#			Function:	get.seq.ids.matches
#######################################
get.seq.ids.matches <- function(from.list, to.list, show=FALSE) {
	# final intersect matrix ordered by 'to.list'

	from.apts <- get.aptamers(unique(from.list))
	to.apts <- get.aptamers(unique(to.list))

	from.list.seq.ids <- sapply(from.apts, apt2seqid)		# sgf modified apt2seqid 7/30/2014
	to.list.seq.ids <- sapply(to.apts, apt2seqid)

	inter <- intersect(to.list.seq.ids, from.list.seq.ids)

	L1 <- match.seq.ids(inter, from.apts, order.by.first=TRUE, include.response=FALSE)
	L2 <- match.seq.ids(inter, to.apts, order.by.first=TRUE, include.response=FALSE)

	if ( 'No matches' %in% c(L1,L2) )
		return("No matches between lists")

	call <- match.call()
	M <- data.frame(L1, L2, stringsAsFactors=FALSE) %names% c(call$from.list,call$to.list)

	if ( show ) M else invisible(M)

}
#### ---- END FUNCTION ---- ####

# ---- created on: 2013-10-18 17:07:15
