# --------------------
# Revision Info
# --------------------
# $Revision: 15971 $
# $Author: sfield $
# $LastChangedDate: 2015-02-05 11:38:13 -0700 (Thu, 05 Feb 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/fix.header.meta.R $
#######################################
#       Function: fix.header.meta
#######################################
fix.header.meta <- function(X) {
	# X = attributes

	X$Header.Meta$COL_DATA$Name <- names(X$Col.Meta)
	X$Header.Meta$COL_DATA$Type <- sapply(names(X$Col.Meta), function(type) {
													  idx <- which(X$Header.Meta$COL_DATA$Name == type)
													  X$Header.Meta$COL_DATA$Type[ idx ]
												})

	# ensure Header.Meta$ROW_DATA == meta data of adat (if child atts intact)
	dat.meta <- X$names[ seq.regex(X$names) < 0 ]		# get meta from attributes
	X$Header.Meta$ROW_DATA$Name <- dat.meta
	X$Header.Meta$ROW_DATA$Type <- sapply(dat.meta, function(type) {
													  idx <- which(X$Header.Meta$ROW_DATA$Name == type)
													  X$Header.Meta$ROW_DATA$Type[ idx ]
												})


	if ( 'CreateDateHistory' %in% names(X$Header.Meta$HEADER) )
		X$Header.Meta$HEADER$CreateDateHistory <- paste(X$Header.Meta$HEADER$CreatedDate, X$Header.Meta$HEADER$CreateDateHistory, sep='\t') 
	else 
		X$Header.Meta$HEADER$CreateDateHistory <- X$Header.Meta$HEADER$CreatedDate 

	if ( 'CreateByHistory' %in% names(X$Header.Meta$HEADER) ) 
		X$Header.Meta$HEADER$CreateByHistory <- paste(X$Header.Meta$HEADER$CreatedBy, X$Header.Meta$HEADER$CreateByHistory, sep='\t')
	else 
		X$Header.Meta$HEADER$CreateByHistory <- X$Header.Meta$HEADER$CreatedBy


	# version number stuff
	if ( !'Version' %in% names(X$Header.Meta$HEADER) || X$Header.Meta$HEADER$Version != '1.2' ) {
		X$Header.Meta$HEADER$Version<- '1.2'
		cat(sprintf('*  Writing adat version .............. %s\n', '1.2'))
	}

	X$Header.Meta$HEADER$CreatedDate <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
	X$Header.Meta$HEADER$CreatedBy <- paste(Sys.getenv('USER'), 'using write.adat.R $Revision: 15971 $:', sep=' ')

	return(X)

}
#### ---- END FUNCTION ---- ####

# ---- created on 2013-03-21 17:36:29
