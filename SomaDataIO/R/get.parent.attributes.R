# --------------------
# Revision Info
# --------------------
# $Revision: 16288 $
# $Author: sfield $
# $LastChangedDate: 2015-03-31 10:54:06 -0600 (Tue, 31 Mar 2015) $
# $URL: svn://kong.sladmin.com/svn-repository/BioInformatics/R/trunk/SomaData/R/get.parent.attributes.R $
###############################################
#       Function: get.parent.attributes
###############################################
get.parent.attributes <- function(df, parent=NULL, space=.GlobalEnv) {

	#  if parent is known   #
	if ( !is.null(parent) ){ 
		cat(sprintf('\n*  Using attributes from data frame passed to parent arg: %s\n', deparse(substitute(parent))))
	if ( !any(rn(df) %in% rn(parent)) )
		warning('Row names of parent and child do not match!')
	return(attributes(parent))
  }

  ###################################
  # get dfs & lists
  env.dfs <- ls(space)[sapply(ls(space), function(x) is.data.frame(get(x)))]
  env.lists <- setdiff(ls(space)[sapply(ls(space), function(x) is.list(get(x)))], env.dfs)
  
  # get actual entrys #
  env.dfs <- lapply(env.dfs, function(f) get(f)) %names% env.dfs
  env.lists <- lapply(env.lists, function(f) get(f)) %names% env.lists
  env.lists <- unlist(env.lists, recursive=FALSE)			# move sublists up one level
 
  # combine dfs and lists of dfs  # 
  all.dfs <- c(env.dfs, env.lists)   

  # ---------------------------------------- #
     #      subfunction for determining parents                                      #
     is.parent <- function(child, parent) {
       df <- is.data.frame(parent)
       header <- all(c("Header.Meta","Col.Meta") %in% names(attributes(parent)))
       RN <- any(rn(child) %in% rn(parent))
       df && header && RN
     }
  # ---------------------------------------- #

  #print(sapply(all.dfs, is.parent, child=df)) 

  filtered.dfs <- all.dfs[sapply(all.dfs, is.parent, child=df)]
  names(filtered.dfs) <- sub("\\.","$",names(filtered.dfs))
  LF <- length(filtered.dfs)

	if ( LF == 0 ) {
		print(paste('Original ADAT not found in',deparse(substitute(space)),'workspace!'))
		cat(sprintf('Original ADAT not found in %s workspace!\n', deparse(substitute(space)) ))
		file <- readline('Please give path to the original ADAT file (no quotes) ... ')
		cat('*  Rebuilding ADAT attributes from original ...\n')
		parent.df <- read.adat(file, remove.buffer=TRUE, replace.names=TRUE)
		parent.atts <- attributes(parent.df)
	}
	else if ( LF == 1) {
		cat(sprintf('*  Found parent ... using: %s\n', names(filtered.dfs)))
		parent.atts <- attributes(filtered.dfs[[1]])
	} 
	else {
		cat(LF, 'data frames could be the parent; Please choose one of:\n')
		sapply(1:LF, function(d) cat(sprintf('  %s ... %i\n', names(filtered.dfs)[d], d))) 
		choose <- as.numeric(readline("The parent is (#): "))
		cat(sprintf("Using: %s\n", names(filtered.dfs)[choose]))
		parent.atts <- attributes(filtered.dfs[[choose]])
	}
   
	parent.atts

}





