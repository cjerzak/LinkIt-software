#' LinkIt
#' 
#' Record linkage description. 
#' 
#' @usage 
#' 
#' LinkIt(x,y,by,...)
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
#' @export

LinkIt <- function(x,y,by=NULL, by.x = NULL,by.y=NULL,
                     fuzzy_step = T, force_unique = T, parallelize = T, 
                     control = list(RemoveCommonWords = T, 
                                    ToLower = T,
                                    NormalizeSpaces = T,
                                    RemovePunctuation = T,
                                    x.stopwordcutoff = 0.1,
                                    y.stopwordcutoff = 0.1,
                                    PreprocessingFuzzyThreshold=0.20) ){ 
  require(tm,quietly=T)
  require(fuzzyjoin,quietly=T)
  require(stringdist)
  require(data.table,quietly=T)
  require(stringr)
  browser() 
  LT_d <- directory[,.(alias_name,alias_id,canonical_id)]
  #coerce to data.table
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
      #3:     1      V3 hancock
      #4:     1      V4  county
      #5:     2      V1 utility

    x.stop.words = x.words[,.(word.freq=.N/nrow(x)),word][
      word.freq > control$x.stopwordcutoff,word
    ]
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
            #get the name we want to fuzzy match against the directory
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
          my_matched_inner[counter_,] <- match
        } 
        colnames(my_matched_inner) <- names(match)
        return( my_matched_inner )  
        } 
        my_matched = do.call(rbind,loop_)
      } 
      
      if(parallelize == F){ 
        for(i in 1:n_iters){ 
          #this implementation may seem a little weird  but is fast 
          if(key_ == "x"){ 
            #get the name we want to fuzzy match against the directory
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
    
    if(T == T){
      x_matched = FastFuzzyMatch(key_ = "x",parallelize=parallelize)
      y_matched = FastFuzzyMatch(key_ = "y",parallelize=parallelize)
      
      x$ALIAS_FUZZYMATCHED <- x_matched[,"alias_name"]
      y$ALIAS_FUZZYMATCHED <- y_matched[,"alias_name"]
      x$ID_FUZZYMATCHED <- x_matched[,"canonical_id"]
      y$ID_FUZZYMATCHED <- y_matched[,"canonical_id"]
      f2n <- function(.){as.numeric(as.character(.))}
      x_matched[,"stringdist"] <- f2n(x_matched[,"stringdist"] ) #/ c( sapply(x_matched[,1],nchar) ) 
      y_matched[,"stringdist"] <- f2n(y_matched[,"stringdist"] ) #/ c( sapply(y_matched[,1],nchar) ) 
      x_red = x[f2n(x_matched[,"stringdist"])<control$PreprocessingFuzzyThreshold,]
      y_red = y[f2n(y_matched[,"stringdist"])<control$PreprocessingFuzzyThreshold,]
    } 

  } 
  #x[grep(x$NM_LGL,pattern="comerica"),]
  #y[grep(y$comnam,pattern="comerica"),] 
  #directory[grep(directory$alias_name,pattern="comerica"),]
  z = merge(x  = x_red,
            y  = y_red,
            by = "ID_FUZZYMATCHED")
  return(  z   ) 
}

trigram_index <- function(phrase,phrasename='phrase.no'){
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

data("LinkIt_directory_trigrams", "LinkIt_directory_trigrams", envir=environment())
#.onLoad <- function(libname, pkgname) {
#data("LinkIt_directory_trigrams.Rdata", "LinkIt_directory_trigrams.Rdata", package=pkgname, envir=parent.env(environment()))
#}

FastFuzzyMatch_public <- function(x,y,by.x, by.y, parallelize = T){
  n_iters = max(nrow(x), nrow(y))
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
        #get the name we want to fuzzy match against the directory
        my_entry = x[i][[by.x]]
        #get the trigrams of this name
        my_entry_trigrams = x_index[the.row==i,trigram]
        
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
        my_matched_inner[counter_,] <- match
      } 
      colnames(my_matched_inner) <- names(match)
      return( my_matched_inner )  
    } 
    my_matched = do.call(rbind,loop_)
  } 

  return( my_matched )
}