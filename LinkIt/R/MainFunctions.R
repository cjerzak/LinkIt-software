#' LinkIt
#'
#' Record linkage description.
#'
#' @usage
#'
#'
#'
#' LinkIt(x,y,by,...)
#'
#' @param x,y data frames to be merged
#'
#' @param by,by.x,by.y specifications of the columns used for merging.
#'
#' @param fuzzy_step A Boolean (T/F) indicating whether an additional
#'   pre-processing step with the LinkedThem database. Default option is TRUE.
#'
#' @param control A list specifying how to process the alias text. See
#'   ``Details''.
#'
#' @return z The merged data frame.
#' @export
#'
#' @details LinkIt can automatically process the alias text for each dataset. In
#' `Control', users can specify the following options.
#'
#' Set 'RemoveCommonWords' to TRUE to remove common words (those appearing in >
#' 10\% of aliases).
#'
#' Set `NormalizeSpaces' to TRUE to remove hanging whitespaces.
#'
#' Set `RemovePunctuation' to TRUE to remove punctuation.
#'
#' Set `ToLower' to TRUE to ignore case.
#'
#' Set 'PreprocessingFuzzyThreshold' to some number between 0 and 1 to specify
#' the threshold for the pre-processing fuzzy matching step.
#'
#' @export


LinkIt <- function(x,y,by=NULL, by.x = NULL,by.y=NULL,
                     fuzzy_step = T, force_unique = T, parallelize = T, 
                     max.n.x = 5, max.n.y = 5,
                     control = list(RemoveCommonWords = T, 
                                    ToLower = T,
                                    NormalizeSpaces = T,
                                    RemovePunctuation = T,
                                    x.stopwordcutoff = 0.1,
                                    y.stopwordcutoff = 0.1,
                                    PreprocessingFuzzyThreshold=0.20,
                                    FuzzyThreshold = 0.10),
                   browser = F){ 
  require(tm,quietly=T)
  #require(fuzzyjoin,quietly=T)
  require(stringdist, quietly = T) 
  require(stringr)
  
  if(!"directory_LinkIt" %in% ls(envir = globalenv())){ 
  temp1 <- tempfile()
  download.file("https://github.com/cjerzak/LinkIt-software/raw/master/directory_data.zip",temp1)
  temp = unzip(temp1)
  load(temp[1]);load(temp[3])
  try(file.remove(temp),T) 
  assign("directory_trigrams", as.data.table(directory_trigrams), envir=globalenv())
  assign("directory_LinkIt", as.data.table(directory), envir=globalenv())
  rm(directory)
  }
  if(browser == T){browser()}

  x = cbind(1:nrow(x),x);colnames(x)[1] <- 'Xref__ID'
  y = cbind(1:nrow(y),y);colnames(y)[1] <- 'Yref__ID'
  by_x_orig = x[[by.x]] ; by_y_orig = y[[by.y]] 
  names(by_x_orig) <- x$Xref__ID;names(by_y_orig) <- y$Yref__ID
  y$UniversalMatchCol <- x$UniversalMatchCol <- NA 
  colnames_x_orig = colnames(x); colnames_y_orig = colnames(y)
  LT_d <- directory_LinkIt[,c("alias_name","canonical_id")]
  
  #PREPROCESSING 
  x = as.data.table(x); y = as.data.table(y) 
  if(!is.null(by)){by.x <- by.y <- by}
  if(control$ToLower == T){
    set(x,NULL,by.x,tolower(x[[by.x]]))
    set(y,NULL,by.y,tolower(y[[by.y]]))
    LT_d[,alias_name := tolower(alias_name)]
  }
  if(control$NormalizeSpaces == T){
    set(x,NULL,by.x,
        str_replace_all(
        x[[by.x]],
        pattern="\\s+",
        replace=' '))
    set(y,NULL,by.y,
            str_replace_all(
              y[[by.y]],
              pattern="\\s+",
              replace=' '))
    LT_d[,alias_name := str_replace_all(alias_name,pattern="\\s+", replace = " ")]
  }
  if(control$RemovePunctuation == T){
    set(x,NULL,by.x,str_replace_all(x[[by.x]],"\\p{P}",""))
    set(y,NULL,by.y,str_replace_all(y[[by.y]],"\\p{P}",""))
    LT_d[,alias_name := str_replace_all(alias_name,"\\p{P}","")]
  }
  if(control$RemoveCommonWords == T){
    #get a list of all the words as a data table
    x.words = setDT(tstrsplit(x[[by.x]]," "),keep.rownames = T)
    #identify the word by the row it is in
    x.words[,x.rowid:=.I]
    #melt this down
    x.words = melt(x.words,id.vars='x.rowid',na.rm=T,variable.name = '_no',value.name='word')
    #now x.words looks something like this 
      #x.rowid word_no    word
      #1:     1      V1    bank
      #2:     1      V2      of

    x.stop.words = x.words[,.(word.freq=.N/nrow(x)),word][word.freq > control$x.stopwordcutoff,word ]
    #now do the same for y
    y.words = setDT(tstrsplit(y[[by.y]]," "),keep.rownames = T)
    y.words[,y.rowid:=.I]
    y.words = melt(y.words,id.vars='y.rowid',na.rm=T,variable.name = '_no',value.name='word')
    y.stop.words = y.words[,.(word.freq=.N/nrow(x)),word][
      word.freq > control$y.stopwordcutoff,word
      ]
    #now drop the stop words
    #make sure there are word boundary tests
    x.regex = paste0(paste0("\\b",x.stop.words,"\\b"),collapse="|")
    set(
      x,,by.x,
      trimws(str_replace_all(str_replace_all(x[[by.x]],x.regex," "),"\\s+"," "))
    )
    y.regex = paste0(paste0("\\b",y.stop.words,"\\b"),collapse="|")
    set(
      y,,by.y,
      trimws(str_replace_all(str_replace_all(y[[by.y]],x.regex," "),"\\s+"," "))
    )
  }
  
  #drop duplicates after pre-process 
  LT_d = LT_d[!duplicated(alias_name) & trimws(alias_name)!='',]
  LT_index = trigram_index(LT_d$alias_name,"lt_d.row")
  
  #specify ID_match for the exact/fuzzy matching 
  x$UniversalMatchCol <- as.character(x[[by.x]]); y$UniversalMatchCol = as.character( y[[by.y]] )  
  
  #exact matching as first step  
  z_exact <- base::merge(x    = as.data.frame(x),
                         y    = as.data.frame(y),
                         by.x = "UniversalMatchCol",
                         by.y = "UniversalMatchCol")
  #if(length(z_exact$Xref__ID) > 0){ x = subset(x,x$Xref__ID %in% z_exact$Xref__ID) } 
  #if(length(z_exact$Xref__ID) > 0){ y = subset(y,y$Yref__ID %in% z_exact$Yref__ID)  } 

  x_index = trigram_index(x[[by.x]],"the.row")
  y_index = trigram_index(y[[by.y]],'the.row')

  if(fuzzy_step == T){ 
    FastFuzzyMatch <- function(key_,parallelize = T){
      if(key_ == "x"){ n_iters = nrow(x)}
      if(key_ == "y"){ n_iters = nrow(y)}
      my_matched = matrix(NA,nrow = n_iters,ncol=4)
      
      if(parallelize == T){ 
        require("foreach",quietly=T); require("doMC",quietly=T)
        ncl = detectCores()
        split_list = round(seq(0.5,n_iters,length.out = ncl+1))
        split_list = as.numeric(cut(1:n_iters,breaks=split_list))
        split_list = sapply(1:ncl, function(as){ list(which(split_list ==as))})
        if(length(unlist(split_list)) != n_iters){browser()}
        cl<-registerDoMC(ncl);
        loop_ <- foreach(outer_i = 1:ncl) %dopar% {
          counter_ <- 0 
          my_matched_inner = matrix(NA,nrow = length(split_list[[outer_i]]),ncol=4)
          for(i in split_list[[outer_i]]){ 
          counter_ = counter_ + 1 
          if(key_ == "x"){ 
            #get the name we want to fuzzy match against the directory_LinkIt
            my_entry = x[i][[by.x]]
            #get the trigrams of this name
            my_entry_trigrams = x_index[the.row==i,trigram]
          } 
          if(key_ == "y"){ 
            my_entry = y[i][[by.y]]
            my_entry_trigrams = y_index[the.row==i,trigram]
          } 
          
          #find the set of entries in LT_d that have some common trigram
          LT_entries = unique(LT_index[trigram %in% my_entry_trigrams,lt_d.row])
          #calculate the nearest match according to string distance
          match = unlist(LT_d[LT_entries,.(
            my_entry = my_entry,
            alias_name,
            stringdist = stringdist(my_entry,alias_name,method="jw"),
            canonical_id)][
              order(stringdist)[1]
              ],recursive = F,use.names=F)
          my_matched_inner[counter_,] <- match
          } 
        colnames(my_matched_inner) <- c("my_entry", "alias_name", "stringdist", "canonical_id")
        return( my_matched_inner )  
        } 
        my_matched = do.call(rbind,loop_)
      } 
      
      if(parallelize == F){ 
        for(i in 1:n_iters){ 
          #this implementation may seem a little weird  but is fast 
          if(key_ == "x"){ 
            #get the name we want to fuzzy match against the directory_LinkIt
            my_entry = x[i][[by.x]]
            #get the trigrams of this name
            my_entry_trigrams = x_index[the.row==i,trigram]
          } 
          if(key_ == "y"){ 
            my_entry = y[i][[by.y]]
            my_entry_trigrams = y_index[the.row==i,trigram]
          } 
          
          #find the set of entries in LT_d that have some common trigram
          LT_entries = unique(LT_index[trigram %in% my_entry_trigrams,lt_d.row])
          #calculate the nearest match according to string distance
          match = unlist(LT_d[LT_entries,.(
            my_entry = my_entry,
            alias_name,
            stringdist = stringdist(my_entry,alias_name,method="jw"),
            canonical_id)][
              order(stringdist)[1]
              ])
          try_ = try(my_matched[i,] <- match,T)
          my_matched[i,] <- match
        } 
        colnames(my_matched) <- names(match)
      } 
      return( my_matched )
    }
    
    {
      f2n <- function(.){as.numeric(as.character(.))}
      for(key_ in c("x", "y")){ 
      (eval(parse(text=sprintf("%s_matched = FastFuzzyMatch(key_ = '%s',parallelize=parallelize)", key_,key_))))
      (eval(parse(text=sprintf("%s$ALIAS_FUZZYMATCHED <- %s_matched[,'alias_name']", key_,key_))))
      (eval(parse(text=sprintf("%s$ID_MATCH <- %s_matched[,'canonical_id']",key_,key_))))
      (eval(parse(text=sprintf("%s$stringdist <- %s_matched[,'stringdist']", key_,key_))))
      (eval(parse(text=sprintf("%s_matched[,'stringdist'] <- f2n(%s_matched[,'stringdist'] ) ", key_,key_))))
      (eval(parse(text=sprintf("%s_red = %s[f2n(%s_matched[,'stringdist'])<control$PreprocessingFuzzyThreshold,]", key_,key_,key_))))
      } 
    }
  }
  
  #fuzzy match 
  z_fuzzy = FastFuzzyMatch_public(x=x[,..colnames_x_orig], y=y[,..colnames_y_orig], 
                                  by.x = "UniversalMatchCol", by.y = "UniversalMatchCol",
                                  method = "jw", max_dist = control$FuzzyThreshold,browser=F)
  colnames(z_fuzzy)[colnames(z_fuzzy) == "stringdist"] <- "stringdist_fuzzy"

  #linkit match 
  z = as.data.frame(merge(x  = x_red,
                          y  = y_red,
                          by = "ID_MATCH"))
  if(nrow(z_exact) > 0){ 
    z_exact$stringdist_fuzzy <- 0  ; z = rbind.fill(z,z_exact)[,colnames(z)]
  }
  if(nrow(z_fuzzy) > 0){ 
    z = rbind.fill(z,z_fuzzy)[,colnames(z)]
  }
  z =  z[!duplicated(  apply(z[,c("Xref__ID","Yref__ID")],1,function(x){paste(x,collapse="")})), ]
  z  = z[,!colnames(z) %in% c("ID_MATCH.x", "ID_MATCH.y")]
  
  tab_x = table(z$Xref__ID)
  z$x_metric_comparison = apply(cbind(z$stringdist.x,z$stringdist_fuzzy),1,function(zs){max(zs,na.rm=T)})
  z$y_metric_comparison = apply(cbind(z$stringdist.y,z$stringdist_fuzzy),1,function(zs){max(zs,na.rm=T)})
  z$metric_comparison = f2n(z$x_metric_comparison) + f2n(z$y_metric_comparison) 
  
  #overmatched x's 
  { 
  overmatched_x = names(tab_x)[tab_x>max.n.x]
  z_good = z[!z$Xref__ID %in% overmatched_x,]
  z_bad = z[z$Xref__ID %in% overmatched_x,]
  z_bad = do.call(rbind, sapply(overmatched_x,function(this_x){ 
    z_bad_this_x_keep = z_bad[z_bad$Xref__ID %in% this_x,]
    z_bad_this_x_keep = z_bad_this_x_keep[order(z_bad_this_x_keep$metric_comparison),]
    z_bad_this_x_keep = z_bad_this_x_keep[1:max.n.x,]
    return( list(z_bad_this_x_keep ))
  } )) 
  z = rbind(z_good,z_bad)
  } 
  
  #overmatched y's 
  { 
    tab_y = table(z$Yref__ID)
    overmatched_y = names(tab_y)[tab_y>max.n.y]
    z_good = z[!z$Yref__ID %in% overmatched_y,]
    z_bad = z[z$Yref__ID %in% overmatched_y,]
    z_bad = do.call(rbind,sapply(overmatched_y,function(this_y){ 
      z_bad_this_y_keep = z_bad[z_bad$Yref__ID %in% this_y,]
      z_bad_this_y_keep = z_bad_this_y_keep[order(z_bad_this_y_keep$metric_comparison),]
      z_bad_this_y_keep = z_bad_this_y_keep[1:max.n.y,]
      return( list(z_bad_this_y_keep ))
    } )) 
    z = rbind(z_good,z_bad)
  } 

  z  = z [,colnames(z)[colnames(z) %in% c(colnames(x ),colnames(y ))]]
  z = z[,!colnames(z) %in% "UniversalMatchCol"]
  
  #undo modifications to names for processing 
  z[[by.x]] <- by_x_orig[z$Xref__ID]
  z[[by.y]] <- by_y_orig[z$Yref__ID]
  return(  z   ) 
}

trigram_index <- function(phrase,phrasename='phrase.no',browser=F){
  if(browser==T){browser()}
  require(plyr)

  DT=data.table(phrase,phrase.no=1:length(phrase))
  t = DT[,.(phrase,phrase.no,phrase.length = nchar(phrase))][
    data.table(start_pos=1:100),
    .(phrase,phrase.no,start_pos),
    on="phrase.length>=start_pos",
    nomatch=0,allow.cartesian=T][
      order(phrase.no)]
  t[,end_pos := pmin(start_pos+2,nchar(phrase))]
  directory_trigrams= t[start_pos==1 | start_pos+2 == end_pos, 
                        .(
                          trigram=substr(phrase,start_pos,end_pos),
                          phrase.no)]
  setkey(directory_trigrams,trigram)
  colnames(directory_trigrams) = c("trigram",phrasename)
  return(directory_trigrams)
}


#' FastFuzzyMatch_public
#' 
#' Record linkage description. 
#' 
#' @usage 
#' 
#' FastFuzzyMatch_public(x,y,by,...)
#' 
#' @param x,y data frames to be merged  
#' 
#' @param by,by.x,by.y specifications of the columns used for merging. 
#' 
#' @param fuzzy_step A Boolean (T/F) indicating whether an additional pre-processing step
#' with the LinkedThem database. Default option is TRUE. 
#' 
#' @param control A list specifying how to process the alias text. See ``Details''. 
#' 
#' @return z The merged data frame. 
#' @export 
#' 
#' @details 
#' LinkIt can automatically process the alias text for each dataset. In `Control', users can specify the following options. 
#' 
#' Set 'RemoveCommonWords' to TRUE to remove common words (those appearing in > 10\% of aliases).
#' 
#' Set `NormalizeSpaces' to TRUE to remove hanging whitespaces.
#' 
#' Set `RemovePunctuation' to TRUE to remove punctuation. 
#' 
#' Set `ToLower' to TRUE to ignore case. 
#' 
#' Set 'PreprocessingFuzzyThreshold' to some number between 0 and 1 to specify the threshold for the pre-processing fuzzy matching step. 
#'
#' 
#' @import stringdist
#' @import plyr
#' 
#' @export
#' 
FastFuzzyMatch_public <- function(x,y,by.x, by.y, return_stringdist = T, 
                                  method = "jw", max_dist = 0.20,browser=F){
  require(stringdist, quietly = T) 
  if(browser == T){browser()}
  #WARNING: X SHOULD ALWAYS BE THE LARGER SET 
  if(nrow(x) < nrow(y)){stop("X SHOULD ALWAYS BE THE LARGER SET")}
  if(by.x == by.y){
    colnames(x)[colnames(x) == by.x] <- paste(by.x, ".x", sep = "")
    colnames(y)[colnames(y) == by.y] <- paste(by.y, ".y", sep = "")
    by.x = paste(by.x, ".x", sep = "");by.y = paste(by.y, ".y", sep = "")
  }
  x[[by.x]] <- tolower(x[[by.x]] )
  y[[by.y]] <- tolower(y[[by.y]] )
  x_index = trigram_index(x[[by.x]],"the.row")
  y_index = trigram_index(y[[by.y]],"the.row")
  n_iters = max(nrow(x), nrow(y))
  my_matched = matrix(NA,nrow = n_iters,ncol=4)
  { 
    require("foreach",quietly=T); require("doMC",quietly=T)
    ncl = detectCores()
    split_list = round(seq(0.5,n_iters,length.out = ncl+1))
    split_list = as.numeric(cut(1:n_iters,breaks=split_list))
    split_list = sapply(1:ncl, function(as){ list(which(split_list ==as))})
    if(length(unlist(split_list)) != n_iters){browser()}
    cl<-registerDoMC(ncl);
    loop_ <- foreach(outer_i = 1:ncl) %dopar% {
      counter_ <- 0 
      my_matched_inner = matrix(NA,nrow = length(split_list[[outer_i]]),ncol=3)
      for(i in split_list[[outer_i]]){ 
        counter_ = counter_ + 1 
        
        #get the name we want to fuzzy match against the directory_LinkIt
        my_entry = x[i][[by.x]]
        #get the trigrams of this name
        my_entry_trigrams = x_index[the.row==i,trigram]
        
        #find the set of entries in LT_d that have some common trigram
        LT_entries = unique(x_index[trigram %in% my_entry_trigrams,the.row])
        
        #calculate the nearest match accordfng to string distance
        eval(parse(text=sprintf("match_ = unlist(y[LT_entries,.(
            my_entry = my_entry,%s,
            stringdist = stringdist(my_entry,%s,method=method))][
              order(stringdist)[1] ])", by.y,by.y) ) ) 
        my_matched_inner[counter_,] <- match_
      } 
      colnames(my_matched_inner) <- names(match)
      return( my_matched_inner )  
    } 
    my_matched = do.call(rbind,loop_)
  } 

  colnames(my_matched) <- c("MATCH_X", "MATCH_Y", "stringdist")
  x = cbind(x,my_matched)
  z = cbind(x, y[match(x[["MATCH_Y"]],y[[by.y]])])
  z = z[as.numeric(as.character(z$stringdist))<max_dist,]
  if(return_stringdist == F){ z = as.data.frame(z)[,!colnames(z) %in% colnames(my_matched)]}
  if(return_stringdist == T){ z = as.data.frame(z)[,!colnames(z) %in% colnames(my_matched) | colnames(z) == "stringdist"]}

  return( z )
}

