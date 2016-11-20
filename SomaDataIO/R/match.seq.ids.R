# --------------------
# Revision Info
# --------------------
# $Revision: 16557 $
# $Author: sfield $
# $LastChangedDate: 2015-06-05 10:50:20 -0600 (Fri, 05 Jun 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/match.seq.ids.R $
#######################################
#       Function: match.seq.ids
#######################################
match.seq.ids <- function(from.list, to.list, order.by.first=TRUE, include.response=FALSE) {

	new.list <- c()
	to.apts <- get.aptamers(to.list)
	from.apts <- get.aptamers(from.list)
	to.list.seq.ids <- sapply(to.apts, apt2seqid)			# sgf modified apt2seqid 7/30/2014
	from.list.seq.ids <- sapply(from.apts, apt2seqid)
	#print(to.list.seq.ids)
	#print(from.list.seq.ids)

	if (order.by.first)
		order.list <- intersect(from.list.seq.ids, to.list.seq.ids)
	else
		order.list <- intersect(to.list.seq.ids, from.list.seq.ids)

	if ( length(order.list) == 0 )
		return("No matches")

	for ( seq.id in order.list ) {
		spot <- to.list.seq.ids %in% seq.id
		new.list <- c(new.list, to.apts[spot])
	}

	if ( include.response && "Response" %in% from.list && "Response" %in% to.list )
		new.list <- c(new.list, "Response")

	new.list

}
#######------- END FUNCTION -------#######
