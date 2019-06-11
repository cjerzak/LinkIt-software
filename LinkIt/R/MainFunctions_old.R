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
#' 

load("LinkIt-software/data/bank_research.data")
by.x='NM_LGL'
by.y='comnam'


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
  

LinkIt <- function(x,y,by=NULL, by.x = NULL,by.y=NULL,
                     fuzzy_step = T, 
                     control = list(RemoveCommonWords = T, 
                                    ToLower = T,
                                    NormalizeSpaces = T,
                                    RemovePunctuation = T,
                                    x.stopwordcutoff = 0.1,
                                    y.stopwordcutoff = 0.1,
                                    PreprocessingFuzzyThreshold=0.5)){ 
  require(tm,quietly=T)
  require(fuzzyjoin,quietly=T)
  require(stringdist)
  require(data.table,quietly=T)
  require(stringr)
  t0 = proc.time()
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
    # 
    # commonWords_x = prop.table(table(unlist(lapply(strsplit(x[[by.x]],split=" "),unique))))
    # commonWords_x = names(commonWords_x)[commonWords_x > 0.10 ]
    # 
    # commonWords_y = prop.table(table(unlist(lapply(strsplit(y[,by.y],split=" "),unique))))
    # commonWords_y = names(commonWords_y)[commonWords_y > 0.10 ]
    # commonWords_xy <- c(commonWords_x,commonWords_y)
    # for(commonWord in commonWords_xy){ 
    #   x[,by.x] = gsub(x[,by.x],pattern = commonWord, replace = "")
    #   y[,by.y] = gsub(y[,by.y],pattern = commonWord, replace = "")
    #   LT_d$alias_name <- gsub(LT_d$alias_name,pattern=commonWord, replace = "")
    # }
  }
  
  #drop duplicates after pre-process 
  LT_d = LT_d[!duplicated(alias_name) & trimws(alias_name)!='',]
  LT_index = trigram_index(LT_d$alias_name,"lt_d.row")
  x_index = trigram_index(x[[by.x]],"x.row")
  y_index = trigram_index(y[[by.y]],'y.row')
  
  if(fuzzy_step == T){ 
    
    #something like thius
    fuzzymatch_index_trick <- function(i){
      #get the name we want to fuzzy match against the directory
      x_entry = x[i][[by.x]]
      #get the trigrams of this name
      x_entry_trigrams = x_index[x.row==i,trigram]
      #find the set of entries in LT_d that have some common trigram
      LT_entries = unique(LT_index[trigram %in% x_entry_trigrams,lt_d.row])
      #calculate the nearest match according to string distance
      match = LT_d[LT_entries,.(
        x_entry = x_entry,
        alias_name,
        stringdist = stringdist(x_entry,alias_name),
        canonical_id)][
          order(stringdist)[1]
          ]
      return(match)
    }
    set.seed(1)
    rows = sample(1:nrow(x),500)
    t0 = proc.time()
    lapply(rows,fuzzymatch_index_trick)
    t1 = proc.time()
    
    t1 - t0
    
    
    X_temp <- as.data.frame(  x[[by.x]] )  
    stringdist()
    
    colnames(X_temp) <- "alias_name"

    
    
    TEMPX = stringdist_join(x = X_temp,
                               y = LT_d, 
                               by = "alias_name",
                               mode = "left",
                               ignore_case = FALSE, 
                               method = "jw", 
                               max_dist = control$PreprocessingFuzzyThreshold, 
                               distance_col = "dist") %>%  group_by(alias_name.x) %>%   top_n(1, -dist)
    t1 = proc.time()
    t1 - t0
    x$ALIAS_FUZZYMATCHED <- TEMPX$alias_name.y
    
    Y_temp <- as.data.frame(  y[,by.y] )  
    colnames(Y_temp) <- "alias_name"
    TEMPY = stringdist_join(x = Y_temp,
                            y = LT_d, 
                            by = "alias_name",
                            mode = "left",
                            ignore_case = FALSE, 
                            method = "jw", 
                            max_dist = control$PreprocessingFuzzyThreshold, 
                            distance_col = "dist") %>%  group_by(alias_name.x) %>%   top_n(1, -dist)
    y$ALIAS_FUZZYMATCHED <- TEMPY$alias_name.y
  } 
  
  z = merge(x  = x,
            y  = y,
            by = "ALIAS_FUZZYMATCHED")
      
  return(  z   ) 
}


if(T == F){  
  x_red = x
  x_red$ALIAS_FUZZYMATCHED <- NA
  X_temp <- as.data.frame(  x[[by.x]] );colnames(X_temp) <- "alias_name"
  X_temp = stringdist_join(x = X_temp,y = LT_d, by = "alias_name",mode = "inner",
                           ignore_case = FALSE, method = "jw", 
                           max_dist = control$PreprocessingFuzzyThreshold, 
                           distance_col = "dist")
  X_temp[["dist"]][X_temp[["dist"]] > 0] <- rnorm(length(X_temp[["dist"]][X_temp[["dist"]] > 0]),
                                                  mean=0,sd=0.0001)
  X_temp = X_temp %>%  group_by(alias_name.x) %>%   top_n(1, -dist)
  x_red[x_red[[by.x]] %in% X_temp$alias_name.x,][["ALIAS_FUZZYMATCHED"]] <- X_temp$alias_name.y
  x_red = x_red[!is.na(x_red$ALIAS_FUZZYMATCHED),]
  
  y_red = y
  y_red$ALIAS_FUZZYMATCHED <- NA
  Y_temp <- as.data.frame(  y[[by.y]] );colnames(Y_temp) <- "alias_name"
  Y_temp = stringdist_join(x = Y_temp,y = LT_d, by = "alias_name",mode = "inner",
                           ignore_case = FALSE, method = "jw", 
                           max_dist = control$PreprocessingFuzzyThreshold, 
                           distance_col = "dist")
  Y_temp[["dist"]][Y_temp[["dist"]] > 0] <- rnorm(length(Y_temp[["dist"]][Y_temp[["dist"]] > 0]),
                                                  mean=0,sd=0.0001)
  Y_temp = Y_temp %>%  group_by(alias_name.x) %>%   top_n(1, -dist)
  y_red[y_red[[by.y]] %in% Y_temp$alias_name.x,][["ALIAS_FUZZYMATCHED"]] <- Y_temp$alias_name.y
  y_red = y_red[!is.na(y_red$ALIAS_FUZZYMATCHED),]
} 