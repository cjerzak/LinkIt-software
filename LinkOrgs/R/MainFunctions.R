#!/usr/bin/env Rscript

#' LinkOrgs
#'
#' Implements the organizational record linkage algorithms of Jerzak and Libgober (2021). 
#'
#' @usage
#'
#'
#'
#' LinkOrgs(x, y, by ...)
#'
#' @param x,y data frames to be merged
#'
#' @param by,by.x,by.y character vector(s) that specify the column names used for merging data frames `x` and `y`. The merging variables should be organizational names. See `?base::merge` for more details regarding syntax. 
#' 
#' @param algorithm character; specifies which algorithm described in Jerzak and Libgober (2021) should be used. Options are "`markov`", "`bipartite`", and "`ml`". Default is "`ml`", which uses a machine learning approach to predicting the match probability.
#' 
#' @param ReturnDiagnostics logical; specifies whether various match-level diagnostics should be returned in the merged data frame. 
#'
#' @param ... For additional specification options, see
#'   ``Details''.
#'
#' @return `z` The merged data frame.
#' @export
#'
#' @details `LinkOrgs` automatically processes the name text for each dataset (specified by `by`, `by.x`, and/or `by.y`. Users may specify the following options:
#'
#' - Set `DistanceMeasure` to control algorithm for computing pairwise string distances. Options include "`osa`", "`jaccard`", "`jw`". See `?stringdist::stringdist` for all options. (Default is "`jaccard`")
#' 
#' - Set `MaxDist` to control the maximum allowed distance between two matched strings 
#' 
#' - Set `qgram` to control the character-level q-grams used in the distance measure. (Default is `2`)
#' 
#' - Set `RemoveCommonWords` to `TRUE` to remove common words (those appearing in >
#' 10% of aliases). (Default is `FALSE`)
#'
#' - Set `NormalizeSpaces` to `TRUE` to remove hanging whitespaces. (Default is `TRUE`)
#'
#' - Set `RemovePunctuation` to `TRUE` to remove punctuation. (Default is `TRUE`)
#'
#' - Set `ToLower` to `TRUE` to ignore case. (Default is `TRUE`)
#'
#' @example 
#' 
#' #Create synthetic data 
#' x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
#' y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")
#' x <- data.frame("orgnames_x"=x_orgnames)
#' y <- data.frame("orgnames_y"=y_orgnames)
#' 
#' # Perform merge 
#' linkedOrgs <- LinkOrgs(x = x, 
#'                        y = y, 
#'                        by.x = "orgnames_x", 
#'                        by.y = "orgnames_y")
#'                        
#' print( linkedOrgs )
#'
#' @export
#' 
#' @importFrom data.table ":="
#' @import Rfast
#' @import doMC
#' @md

LinkOrgs <- function(x,y,by=NULL, by.x = NULL,by.y=NULL,
                    algorithm = "bipartite",
                    ReturnDiagnostics = F, ReturnProgress = T, 
                    ToLower = T,
                    NormalizeSpaces = T,
                    RemovePunctuation = T,
                    MaxDist = 0.20,
                    DistanceMeasure = "jaccard",
                    qgram = 2,
                   openBrowser = F,ReturnDecomposition = F){ 
  library(plyr); library(dplyr)
  require(tm,quietly=F)
  require(data.table)
  require(stringdist, quietly = F) 
  require(stringr)

  if(algorithm == "ml"){ 
    #myCon = url("https://dl.dropboxusercontent.com/s/zyrbp9cj9s3g3wl/mlClust.Rdata?dl=0"); 
    { 
          myCon = try(url("https://dl.dropboxusercontent.com/s/hy8ilnv0u955oa8/getNumericalContrast.rds?dl=0"),T)
          try(getNumericalContrast <- readRDS(myCon),T)
          try(close(myCon),T);rm(myCon)
          
          myCon = try(url("https://dl.dropboxusercontent.com/s/zyrbp9cj9s3g3wl/myForest.Rdata?dl=0"),T)
          try(load(myCon),F); close(myCon);rm(myCon)
          
          coefMat <- read.csv(file="https://dl.dropboxusercontent.com/s/d89up5488l9ujl1/coefMat.csv?dl=0")
          tmp <- as.character(coefMat[,1]);coefMat <- as.data.frame(coefMat[,-1]); row.names(coefMat) <- tmp; coefMat <- as.matrix( coefMat )
          idf_values <- read.csv(file="https://dl.dropboxusercontent.com/s/bkbhoxac6ai6m9p/idf_values.csv?dl=0")
          tmp <- idf_values[,1]; idf_values <- idf_values[,2];names(idf_values)<- tmp; rm(tmp)
          idf_values <<- idf_values
          median_idf <<- median(idf_values)
          
          try(myForest <<- myForest,T); type_ <- "lasso"
          if(type_=="randomForest"){library(randomForest)}
          if(type_=="lasso"){library(glmnet)}
          predProbMatch <- function(strRef,strPool,
                                    VECS_INPUT_w,HASH_INPUT_w,
                                    VECS_INPUT_s,HASH_INPUT_s ){
          contrastMat <- sapply(strPool,function(ze){list(try(getNumericalContrast(strRef, ze,
                                                                                     wordVecs_w = VECS_INPUT_w, 
                                                                                     hashTab_w = HASH_INPUT_w,
                                                                                     wordVecs_s = VECS_INPUT_s, 
                                                                                     hashTab_s = HASH_INPUT_s
                                                                                     ),T))})
          contrastMat <- do.call(rbind,contrastMat)
            if(type_ == "rforest"){ 
              library(randomForest)
              prob_ = try(predict(myForest,newdata = as.data.frame((contrastMat)),type="prob")[,2],T)
            }
            if(type_ == "lasso"){ 
               prob_ = 1 / (1+exp( - c(cbind(1,contrastMat) %*% coefMat)  ) ) 
            }
            names(prob_) <- strPool
            return( prob_ )  
          }
          stripFxn <- function(ze){ 
            ze <- gsub(ze,pattern="\\)",replace="")
            ze <- gsub(ze,pattern="\\(",replace="")
            ze <- gsub(ze,pattern="\\.",replace="")
            ze <- gsub(ze,pattern="\\,",replace="")
            ze <- gsub(ze,pattern="\\-",replace="")
            ze <- gsub(ze,pattern="\\*",replace="")
          }
    }
  } 
  redownload <- T
  if("directory_LinkIt" %in% ls(envir = globalenv())){
    if(algorithm == "markov" & nrow(directory_LinkIt) == 264320){redownload <- F}
    if(algorithm == "bipartite" & nrow(directory_LinkIt) != 264320){redownload <- F}
  }
  if(redownload & algorithm != "ml"){ 
    temp1 <- tempfile(pattern = "tmp14323512321423231960")
    #thanks to of https://techapple.net/2014/04/trick-obtain-direct-download-links-dropbox-files-dropbox-direct-link-maker-tool-cloudlinker/
    if(algorithm == "bipartite"){download.file("https://dl.dropboxusercontent.com/s/tq675xfnnxjea4d/directory_data_bipartite_thresh40.zip?dl=0",destfile = temp1)}
    if(algorithm == "markov"){download.file("https://dl.dropboxusercontent.com/s/ftt6ts6zrlnjqxp/directory_data_markov.zip?dl=0",destfile = temp1)}
    temp = unzip(temp1,junkpaths=T,exdir = "tmp14323512321423231960")
    load(temp[which(grepl(temp,pattern=sprintf("LinkIt_directory_%s_trigrams.Rdata",algorithm) ))[1]])
    load(temp[which(grepl(temp,pattern=sprintf("LinkIt_directory_%s.Rdata",algorithm) ))[1]])
    try(file.remove(temp),T) 
    assign("directory_trigrams", as.data.table(directory_trigrams), envir=globalenv())
    if(ToLower == T){ directory_trigrams$trigram <- tolower(directory_trigrams$trigram) }
    directory_trigrams = directory_trigrams[!duplicated(paste(directory_trigrams$trigram,
                                                        directory_trigrams$alias_id,collapse="_")),]
    print( sprintf("Directory size: %i aliases",nrow( directory )  ))
    assign("directory_LinkIt", as.data.table(directory), envir=globalenv())
    rm(directory)
  } 
  #load("./directory_data_bipartite_thresh40/LinkIt_directory_bipartite_trigrams.Rdata")
  #load("./directory_data_bipartite_thresh40/LinkIt_directory_bipartite.Rdata")
  #print(  sort( sapply(ls(),function(x){object.size(get(x))}))  )  
  
  if(openBrowser == T){browser()}
  x = cbind(1:nrow(x),x);colnames(x)[1] <- 'Xref__ID'
  y = cbind(1:nrow(y),y);colnames(y)[1] <- 'Yref__ID'
  by_x_orig = x[[by.x]] ; by_y_orig = y[[by.y]] 
  names(by_x_orig) <- x$Xref__ID;names(by_y_orig) <- y$Yref__ID
  y$UniversalMatchCol <- x$UniversalMatchCol <- NA 
  colnames_x_orig = colnames(x); colnames_y_orig = colnames(y)

  #PREPROCESSING 
  x = as.data.table(x); y = as.data.table(y) 
  if(!is.null(by)){by.x <- by.y <- by}
  if(ToLower == T){
    set(x,NULL,by.x,tolower(x[[by.x]]))
    set(y,NULL,by.y,tolower(y[[by.y]]))
    if(algorithm != "ml"){ directory_LinkIt[["alias_name"]] <- tolower(directory_LinkIt[["alias_name"]] ) }
  }
  if(NormalizeSpaces == T){
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
    if(algorithm != "ml"){ directory_LinkIt[["alias_name"]] <- str_replace_all(directory_LinkIt[["alias_name"]],pattern="\\s+", replace = " ") }
  }
  if(RemovePunctuation == T){
    set(x,NULL,by.x,str_replace_all(x[[by.x]],"\\p{P}",""))
    set(y,NULL,by.y,str_replace_all(y[[by.y]],"\\p{P}",""))
    if(algorithm != "ml"){directory_LinkIt[["alias_name"]] <- str_replace_all(directory_LinkIt[["alias_name"]],"\\p{P}","")  }
  }

  #drop duplicates after pre-process 
  if(algorithm != "ml"){
    directory_LinkIt = directory_LinkIt[!duplicated(alias_name) & trimws(alias_name)!='',]
    
    #get trigrams 
    directory_LinkIt_red <- directory_LinkIt[,c("alias_name","canonical_id")]
    dir_tri_index <- trigram_index(as.character(directory_LinkIt_red$alias_name),"dir.row")
    x_tri_index  <- trigram_index(x[[by.x]],"the.row")
    y_tri_index  <- trigram_index(y[[by.y]],'the.row')
    
    #drop components of the big corpus which don't share any trigrams with any entries in {x,y}
    tmp = unique(c(unique(as.character(x_tri_index[,trigram])),unique(as.character(y_tri_index[,trigram]))))
    dir_tri_index = dir_tri_index[trigram %in% tmp,];rm(tmp);setkey(dir_tri_index, trigram)
  }

  #specify ID_match for the exact/fuzzy matching 
  x$UniversalMatchCol <- as.character(x[[by.x]]); y$UniversalMatchCol = as.character( y[[by.y]] )  

  #FAST MATCH --- DOESN'T WORK WITH NAs 
  `%fin%` <- function(x, table) {stopifnot(require(fastmatch));fmatch(x, table, nomatch = 0L) > 0L}

  # first, traditional fuzzy match 
  z_fuzzy <- try(as.data.frame(FastFuzzyMatch(x,  y,
                                              by.x = by.x, 
                                              by.y = by.y,
                                              DistanceMeasure = DistanceMeasure, 
                                              MaxDist = MaxDist,
                                              q = qgram)) ,T)
  colnames(z_fuzzy)[colnames(z_fuzzy) == "stringdist"] <- "stringdist_fuzzy"
  
  #get matches 
  { 
    if(algorithm == "ml"){
      y[[by.y]] <- stripFxn(y[[by.y]])
      x[[by.x]] <- stripFxn(x[[by.x]])
      
      tmp_ <- c(x[[by.x]],y[[by.y]])
      tmp_ <- strsplit(tmp_,split=" ")
      tmp_ <- unique( unlist(tmp_))

      #print("unfound terms");print(head(tmp_[!tmp_ %in% names(idf_values)],25))
      idf_values <- idf_values <- idf_values[which(names(idf_values) %fin% tmp_)]
      rm(tmp_)
      
      require(reticulate)
      try_chars2vec <- try(py_run_string('import chars2vec'),T)
      if(class(try_chars2vec)=="try-error"){
        stop("Please install chars2vec, tensorflow, and keras; see 
             https://github.com/IntuitionEngineeringTeam/chars2vec for details.")
      }
      py_run_string('c2v_model = chars2vec.load_model("eng_50")')
      
      #get w vecs 
      { 
      word_list <<- unique(c(unique(unlist(strsplit(y[[by.y]],split=" "))),
                    unique(unlist(strsplit(x[[by.x]],split=" ")))))
      py_run_string('vecs_w = c2v_model.vectorize_words(r.word_list)')
      vecs_w <- py$vecs_w
      HASHTAB_w <- 1:length(word_list); names(HASHTAB_w) <- word_list
      } 
      
      #get string vecs 
      { 
        str_list <<- unique(c(unique(y[[by.y]]),unique(x[[by.x]])))
        py_run_string('vecs_s = c2v_model.vectorize_words(r.str_list)')
        vecs_s <- py$vecs_s
        HASHTAB_s <- 1:length(str_list)
        names(HASHTAB_s) <- str_list  
      }
      
      ## TESTING FUNCTIONS 
      if(T == F){ 
        x_matched <- stripFxn(z_red_human$Name)
        my_entry <- "apple inc"; key_ <- "x";i=1; match_pool <- y[[by.y]]
        my_entry <- sample( x[[by.x]],1); key_ <- "x";i=1; match_pool <- y[[by.y]]
        my_entry <- sample(x_matched,1); key_ <- "x";i=1; match_pool <- y[[by.y]]
        #HASHTAB["apple"];HASHTAB["inc"];HASHTAB["computer"]
        #matchProb_vec <- predProbMatch(strRef  = "apple inc", strPool = c("apple computer","abeple"),VECS_INPUT = vecs_all, HASH_INPUT = HASHTAB)
        matchProb_vec <- predProbMatch(strRef  = my_entry, strPool = match_pool[1:100],
                                       VECS_INPUT_w = vecs_w, HASH_INPUT_w = HASHTAB_w,
                                       VECS_INPUT_s = vecs_s, HASH_INPUT_s = HASHTAB_s  )
        my_entry;head(sort( matchProb_vec,decreasing=T))
        z_red_human[z_red_human$Name %fin% my_entry, ]
        system.time(replicate(10000,{
        wts_a <- fastmatch::fmatch(c("zuora","zynga"),names(idf_values));
        wts_a <- idf_values[wts_a]
        }))
      }
      
      FastFuzzyMatch_internal <- function(key_){ 
        if(key_ == "x"){ n_iters = nrow(x);match_pool <- y[[by.y]] }; 
        if(key_ == "y"){ n_iters = nrow(y);match_pool <- x[[by.x]] }

        {
          require("doMC",quietly=T);library(foreach)#library(doSNOW);
          ncl <- parallel::detectCores();
          print(sprintf("%s cores",ncl))
          doMC::registerDoMC(ncl)
        }        
        my_matched <- as.data.frame(foreach(i = 1:n_iters, .combine=rbind) %dopar% {
            if(i %% 10==0 & ReturnProgress){write.csv(data.frame("Current Iters"=i,"Total Iters"=n_iters),file='./PROGRESS_LINKIT_ml.csv')}
            if(key_ == "x"){ my_entry = x[i][[by.x]]}
            if(key_ == "y"){ my_entry = y[i][[by.y]]}
            matchProb_vec <- try(predProbMatch(strRef     = my_entry, strPool = match_pool,
                                           VECS_INPUT_w = vecs_w, HASH_INPUT_w = HASHTAB_w,
                                           VECS_INPUT_s = vecs_s, HASH_INPUT_s = HASHTAB_s  ),T) 
            probNonMatch <- try(1-matchProb_vec,T) 
            match_indices <- which(probNonMatch <= MaxDist)
            match_ <- data.frame("my_entry"=NA, "alias_name"=NA,"stringdist"=NA, "canonical_id"= NA)
            match_ <- match_[-1,]
            if(length(match_indices) > 0){ 
              match_ <- data.frame("my_entry"=my_entry, "alias_name"= match_pool[match_indices], 
                           "stringdist"=as.vector(probNonMatch[match_indices]), "canonical_id"= NA)
            }
            return( match_ )
        })
      }
    }
    if(algorithm != "ml"){
      FastFuzzyMatch_internal <- function(key_){
      if(key_ == "x"){ n_iters = nrow(x)}; if(key_ == "y"){ n_iters = nrow(y)}
      my_matched = matrix(NA,nrow = 0,ncol=4)
      colnames(my_matched) <- c("my_entry","alias_name","stringdist","canonical_id")
      
      maxDocSearchThres = 25
      { 
        require("foreach",quietly=T); require("doMC",quietly=T); library(parallel)
        ncl <- 1; split_list <- list(1:n_iters)
        if(n_iters>50){
          ncl = parallel::detectCores()
          print(sprintf("%s cores",ncl))
          split_list = round(seq(0.5,n_iters,length.out = ncl+1))
          split_list = as.numeric(cut(1:n_iters,breaks=split_list))
          split_list = sapply(1:ncl, function(as){ list(which(split_list ==as))})
        }
        cl<-doMC::registerDoMC(ncl);
        loop_ <- foreach(outer_i = 1:ncl) %dopar% {
          counter_ <- 0 
          my_matched_inner = matrix(NA,nrow = 0 ,ncol=4)
          colnames(my_matched_inner) <- c("my_entry","alias_name","stringdist","canonical_id")# alias_name is match name 
          for(i in split_list[[outer_i]]){ 
          counter_ = counter_ + 1 
          if(i %% 100==0 & ReturnProgress){write.csv(data.frame("Current Split"=outer_i,
                                                                "Total Splits"=ncl,
                                                                "Current Iters in Split"=counter_,
                                                                "Total Iters in Split"=length(split_list[[outer_i]])),
                                                     file=sprintf('./PROGRESS_LINKIT_%s.csv',algorithm))}
          if(key_ == "x"){ 
            #get the name we want to fuzzy match against the directory_LinkIt
            my_entry = x[i][[by.x]]
            #get the trigrams of this name
            my_entry_trigrams = x_tri_index[the.row==i,trigram]
          } 
          if(key_ == "y"){ 
            my_entry = y[i][[by.y]]
            my_entry_trigrams = y_tri_index[the.row==i,trigram]
          } 
          
          #WARNING: BIAS IN LONGER TRIGRAM KEYS??? 
          #find the set of entries in directory_LinkIt_red that have some common trigram
          #https://appsilon.com/fast-data-lookups-in-r-dplyr-vs-data-table/
          dir_entries_tab = Rfast::Table(dir_tri_index[.(my_entry_trigrams),dir.row, nomatch = 0L])
          #MinNumSharedTriGrams = ceiling(length(my_entry_trigrams)*0.05);dir_entries_tab <- dir_entries_tab[dir_entries_tab>=MinNumSharedTriGrams]
          takeTopProp = quantile(dir_entries_tab,max(1-maxDocSearchThres/length(dir_entries_tab),0.97));dir_entries_tab = dir_entries_tab[dir_entries_tab>=takeTopProp]
          match_ = (directory_LinkIt_red[f2n(names(dir_entries_tab)),.(
            my_entry = my_entry,
            alias_name,
            stringdist = stringdist(my_entry,alias_name,method=DistanceMeasure,q = qgram),
            canonical_id)][
              which(stringdist<=MaxDist)
              ])
          if(nrow(match_) > 0){ 
            match_ = match_[!duplicated(match_$canonical_id),]
            my_matched_inner <- rbind(my_matched_inner,match_)
          } 
          } 
        colnames(my_matched_inner) <- c("my_entry", "alias_name", "stringdist", "canonical_id")
        return( my_matched_inner )  
        } 
        my_matched = do.call(rbind,loop_)
      } 
      my_matched = my_matched[!duplicated(apply(cbind(my_matched[["my_entry"]],my_matched[["canonical_id"]]),1,function(ae){
        paste(ae,collapse="_")})),]
      return( as.data.frame(my_matched) )
      }
      }
    {
      f2n <- function(.){as.numeric(as.character(.))}
      if(algorithm != "ml"){
        xLinked <- FastFuzzyMatch_internal(key_ = 'x')
        yLinked <- FastFuzzyMatch_internal(key_ = 'y')
        xLinked = merge(as.data.frame(x),as.data.frame(xLinked),by.x=by.x,by.y="my_entry",all=F)
        yLinked = merge(as.data.frame(y),as.data.frame(yLinked),by.x=by.y,by.y="my_entry",all=F)
        z_linkIt <- merge(xLinked,yLinked,by="canonical_id",all=F)
      }
      if(algorithm == "ml"){
        keyNot_<-"y";key_ <- "x";if(nrow(x)>nrow(y)){key_ <- "y";keyNot_<-"x"}
        eval(parse(text=sprintf("z_linkIt <- FastFuzzyMatch_internal(key_ = '%s')",key_)))
        z_linkIt[,'stringdist'] <- f2n(z_linkIt[,'stringdist'])
        z_linkIt$stringdist.x <- z_linkIt$stringdist.y <- z_linkIt$stringdist
        eval(parse(text=sprintf("z_linkIt = dplyr::inner_join(as.data.frame(%s), as.data.frame(z_linkIt),by=c('%s'='my_entry'))", key_,eval(parse(text=sprintf("by.%s",key_)))   )))
        eval(parse(text=sprintf("z_linkIt = dplyr::inner_join(as.data.frame(%s),as.data.frame(z_linkIt),by=c('%s'='alias_name'))", keyNot_, eval(parse(text=sprintf("by.%s",keyNot_))) )))
      }
      } 
    }
  
  colnames(z_linkIt)[colnames(z_linkIt) == "canonical_id"] <- "ID_MATCH"

  # bring in fuzzy matches 
  { 
  z_fuzzy$XYref__ID <- paste(z_fuzzy$Yref__ID,
                             z_fuzzy$Xref__ID,sep="__LINKED__")
  z_linkIt$XYref__ID <- paste(z_linkIt$Yref__ID,
                              z_linkIt$Xref__ID,sep="__LINKED__")
  
  #z = merge(z_fuzzy,z_linkIt, by = "XYref__ID",all = T)
  z = rbind.fill(z_fuzzy,z_linkIt)
  
  { # dead with redundant names 
    tmp_ <- gsub(colnames(z),pattern="\\.x",replace="")
    tmp_ <- gsub(tmp_,pattern="\\.y",replace="")
    tmp__ <- tapply(1:ncol(z),tmp_,function(ze){ 
      value_ <- 0
      if(length(ze) == 2){value_ <- mean(z[,ze[1]] == z[,ze[2]],na.rm=T) }
      return( value_ )
    })
    rectify_names <- names((tmp__ == 1)[(tmp__ == 1)])
    colnames(z)[tmp_ %in% rectify_names] <- rectify_names
    z <- z[,!duplicated(colnames(z))]
  } 
  
  inf20 <- function(ze){ if(is.infinite(ze)){ze<-0};ze}
  na20 <- function(ze){ ze[is.na(ze)] <- 0;ze}
  
  #drop duplicates 
  if(nrow(z)>1){ 
    z$minDist <- apply(cbind(z$stringdist.x,z$stringdist.y),1,function(ze){inf20(max(ze,na.rm=T))}) + 
      na20(z$stringdist_fuzzy)
    
    z <- try(do.call(rbind, tapply(1:nrow(z),z$XYref__ID,function(ze){
      z_red <- z[ ze,]
      list(  z_red <- z_red[which.min(z_red$minDist),] )  
    })) , T)
  } 
  if(class(z) == "try-error"){browser()}
  z  = z[,!colnames(z) %in% c("ID_MATCH.x", "ID_MATCH.y")]
  
  if(ReturnDiagnostics == F){ 
    z  = z[,colnames(z)[colnames(z) %in% c(colnames(x ),colnames(y ))]]
    z = z[,!colnames(z) %in% "UniversalMatchCol"]
  }
  
  #undo modifications to names for processing 
  z[[by.x]] <- by_x_orig[z$Xref__ID]
  z[[by.y]] <- by_y_orig[z$Yref__ID]
  z  = z[,!colnames(z) %in% c('Yref__ID', 'Xref__ID')]
  }

  return_ <- z
  if(ReturnDecomposition == T){ return_ = list("z"=z,"z_fuzzy"=z_fuzzy,"z_linkIt"=z_linkIt)  }
  return(  return_ ) 
}

#' FastFuzzyMatch
#' 
#' Performs fast fuzzy matching of strings based on the string distance measure specified in `DistanceMeasure`.
#' 
#' 
#' @usage 
#' 
#' FastFuzzyMatch(x,y,by,...)
#' 
#' @param x,y data frames to be merged  
#' 
#' @param by,by.x,by.y specifications of the columns used for merging. See `?base::merge` for more details regarding syntax. 
#' 
#' @param ... For additional options, see ``Details''. 
#' 
#' @return z The merged data frame. 
#' @export 
#' 
#' @details 
#' LinkIt can automatically process the alias text for each dataset. Users may specify the following options: 
#' 
#' - Set `DistanceMeasure` to control algorithm for computing pairwise string distances. Options include "`osa`", "`jaccard`", "`jw`". See `?stringdist::stringdist` for all options. (Default is "`jaccard`")
#' 
#' - Set `MaxDist` to control the maximum allowed distance between two matched strings 
#' 
#' - Set `qgram` to control the character-level q-grams used in the distance measure. (Default is `2`)
#' 
#' - Set `RemoveCommonWords` to TRUE to remove common words (those appearing in > 10% of aliases). (Default is `FALSE`)
#' 
#' - Set `NormalizeSpaces` to TRUE to remove hanging whitespaces. (Default is `TRUE`)
#' 
#' - Set `RemovePunctuation` to TRUE to remove punctuation. (Default is `TRUE`)
#' 
#' - Set `ToLower` to TRUE to ignore case. (Default is `TRUE`)
#'
#' @example
#' 
#' #' #Create synthetic data 
#' x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
#' y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")
#' x <- data.frame("orgnames_x"=x_orgnames)
#' y <- data.frame("orgnames_y"=y_orgnames)
#' z <- data.frame("orgnames_x"=x_orgnames[1:2], "orgnames_y"=y_orgnames[1:2])
#' z_true <- data.frame("orgnames_x"=x_orgnames, "orgnames_y"=y_orgnames)
#' 
#' # Perform merge 
#' linkedOrgs_fuzzy <- FastFuzzyMatch(x = x, 
#'                        y = y, 
#'                        by.x = "orgnames_x", 
#'                        by.y = "orgnames_y")
#' 
#' @import stringdist
#' @import plyr
#' @import data.table
#' 
#' @export
#' @md 

FastFuzzyMatch <- function(x, y, by = NULL, by.x = NULL, by.y = NULL, return_stringdist = T, onlyUFT = T, 
                           qgram =2, DistanceMeasure = "jaccard", MaxDist = 0.20,openBrowser=F,ReturnProgress=T){
  require(stringdist, quietly = T) 
  if(openBrowser == T){browser()}
  
  #WARNING: X SHOULD ALWAYS BE THE LARGER SET 
  if(is.null(by.x) & is.null(by.y)){by.x <- by.y <- by}
  if(nrow(y)>nrow(x)){ 
    x_old = x; y_old = y;by.y_old = by.y;by.x_old =by.x
    x <- y_old;by.x = by.y_old
    y <- x_old;by.y = by.x_old
    rm(x_old,y_old)
  }
  if(by.x == by.y){
    colnames(x)[colnames(x) == by.x] <- paste(by.x, ".x", sep = "")
    colnames(y)[colnames(y) == by.y] <- paste(by.y, ".y", sep = "")
    by.x = paste(by.x, ".x", sep = "");by.y = paste(by.y, ".y", sep = "")
  }
  y = as.data.table(y)
  x = as.data.table(x)
  x[[by.x]] <- tolower(x[[by.x]] )
  y[[by.y]] <- tolower(y[[by.y]] )
  x_tri_index = trigram_index(x[[by.x]],"the.row")
  y_tri_index = trigram_index(y[[by.y]],"the.row")
  n_iters = max(nrow(x), nrow(y))
  { 
    require("foreach",quietly=T); require("doMC",quietly=T)
    ncl <- 1; split_list <- list(1:n_iters)
    if(n_iters>50){ 
      ncl = parallel::detectCores()
      split_list = round(seq(0.5,n_iters,length.out = ncl+1))
      split_list = as.numeric(cut(1:n_iters,breaks=split_list))
      split_list = sapply(1:ncl, function(as){ list(which(split_list ==as))})
    }
    f2n = function(.){as.numeric(as.character(.))}
    cl<-doMC::registerDoMC(ncl);
    loop_ <- foreach(outer_i = 1:ncl) %dopar% {
      counter_ <- 0  
      my_matched_inner = matrix(NA,nrow = 0,ncol=3)
      colnames(my_matched_inner) <- c("my_entry",by.y,"stringdist")
      for(i in split_list[[outer_i]]){ 
        counter_ = counter_ + 1 
        if(i %% 100==0 & ReturnProgress){write.csv(data.frame("Current Split"=outer_i,
                                                              "Total Splits"=ncl,
                                                              "Current Iters in Split"=counter_,
                                                              "Total Iters in Split"=length(split_list[[outer_i]])),
                                                   file='./PROGRESS_FUZZY.csv')}
        
        
        #get the name we want to fuzzy match against 
        my_entry = x[i,][[by.x]]
        #get the trigrams of this name
        my_entry_trigrams = x_tri_index[the.row==i,trigram]
        
        #find the set of entries in directory_LinkIt_red that have some common trigram
        #LT_entries = unique(x_tri_index[trigram %in% my_entry_trigrams,the.row])
        if(nrow(y)<1e5){ 
          LT_entries = 1:nrow(y)
        } 
        if(nrow(y)>=1e5){ 
          MinNumSharedTriGrams = ceiling(length(my_entry_trigrams)*0.1)
          LT_entries = table(y_tri_index[trigram %in% my_entry_trigrams,the.row])
          LT_entries = f2n(names(LT_entries[LT_entries>=MinNumSharedTriGrams]))
        } 
        
        #calculate the nearest match accordfng to string distance
        match_ = sprintf("y[LT_entries,.(
                         my_entry=my_entry,%s,
                         stringdist = stringdist(my_entry,%s,method=DistanceMeasure,q = qgram))]",by.y,by.y)
        match_ = eval(parse(text=match_))
        if(nrow(match_)>0){
          #match_ = match_[,.(which(stringdist<=MaxDist)) ]
          match_ = as.data.frame(match_)
          match_ = match_[which(match_$stringdist<=MaxDist),]
          my_matched_inner = rbind(my_matched_inner,match_)
        }
      } 
      return( my_matched_inner )  
    } 
    my_matched = do.call(rbind,loop_)
} 
  colnames(my_matched)[1] <- by.x
  myMatched = merge(as.data.frame(x), as.data.frame(my_matched),
                    by.x=by.x,by.y=by.x,all.x = F,all.y=T)
  myMatched = merge(as.data.frame(y), as.data.frame(myMatched),
                    by.x=by.y,by.y=by.y,all.x = F,all.y=T)
  myMatched = as.data.frame( myMatched )
  
  myMatched = myMatched[!duplicated(paste(myMatched[[by.x]],
                                          myMatched[[by.y]],sep="__")),]
  return( myMatched )
  }




#' AssessMatchPerformance
#' 
#' Automatically computes the true/false positive and true/false negative rates based on a ground-truth (preferably human-generated) matched dataset. 
#' 
#' @usage 
#' 
#' AssessMatchPerformance(x,y,by,...)
#' 
#' @param x,y data frames to be merged  
#' 
#' @param by,by.x,by.y character strings specifying of the columns used for merging. 
#' 
#' @param z the merged data frame to be analyzed. Should contain `by`,`by.x`, and/or `by.y` as column names, depending on usage. 
#' 
#' @param z_true a reference data frame containing target/true matched dataset. Should contain `by`,`by.x`, and/or `by.y` as column names, depending on usage. 
#' 
#' @return `ResultsMatrix` A matrix containing the information on the true positive, false positive, 
#' true negative, and false negative rate, in addition to the matched dataset size.  These quantities are calculated based off
#' all possible `nrow(x)*nrow(y)` match pairs. 
#' 
#' @examples 
#' # Create synthetic data 
#' x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
#' y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")
#' x <- data.frame("orgnames_x"=x_orgnames)
#' y <- data.frame("orgnames_y"=y_orgnames)
#' z <- data.frame("orgnames_x"=x_orgnames[1:2], "orgnames_y"=y_orgnames[1:2])
#' z_true <- data.frame("orgnames_x"=x_orgnames, "orgnames_y"=y_orgnames)
#' 
#' # Obtain match performance data 
#' performanceMat <- AssessMatchPerformance(x = x, 
#'                                    y = y, 
#'                                    z = z, 
#'                                    z_true = z_true,
#'                                    by.x = "orgnames_x", 
#'                                    by.y = "orgnames_y")
#' print( performanceMat )
#' 
#' 
#' @export
#' @md 

AssessMatchPerformance = function(x, y, z, z_true, by, by.x=by, by.y=by, openBrowser=F){ 
  if(openBrowser==T){browser()}
  `%fin%` <- function(x, table) {stopifnot(require(fastmatch));fmatch(x, table, nomatch = 0L) > 0L}
  x <- as.matrix(x);y <- as.matrix(y);z <- as.matrix(z);z_true <- as.matrix(z_true);
  totalCombs <- length( unique(x[,by.x]) ) * length( unique(y[,by.y]) )
  ResultsMatrix =  c(matrix(0,nrow=1,ncol=5) )
  names(ResultsMatrix) <- c("TruePositives",
                            "FalsePositives",
                            "FalseNegatives",
                            "TrueNegatives",
                            "MatchedDatasetSize")
  
  #drop remaining duplicates 
  dup_z_ <- duplicated(paste(z[,by.x],z[,by.y],sep= "___"))
  dup_zhuman_ <- duplicated(paste(z_true[,by.x],z_true[,by.y],sep= "___"))
  if(length(dup_z_) > 0 & nrow(z)>1){ z <- z[!dup_z_,] } 
  if(length(dup_z_) > 0 & nrow(z_true)>1){ z_true <- z_true[!dup_zhuman_,]  } 
  
  { 
    z_vec = paste(z[,by.x],z[,by.y],sep="____LINKED____")
    z_true_vec <- paste(z_true[,by.x],z_true[,by.y],sep="____LINKED____")
    z_in_truth <- table(  z_vec %fin% z_true_vec ) 
    truth_in_z <- table(  z_true_vec %fin% z_vec ) 
    NA20 <- function(ze){ if(is.na(ze)){ze <- 0};ze}
    ResultsMatrix["TruePositives"] <- NA20(z_in_truth["TRUE"])
    ResultsMatrix["FalsePositives"] <- NA20(z_in_truth["FALSE"])
    ResultsMatrix["FalseNegatives"] <- NA20(truth_in_z["FALSE"])
    ResultsMatrix["TrueNegatives"] <- totalCombs -  ResultsMatrix["TruePositives"] - ResultsMatrix["FalsePositives"]
  }
  ResultsMatrix["MatchedDatasetSize"] <- nrow(z)
  return( ResultsMatrix  )  
} 

trigram_index <- function(phrase,phrasename='phrase.no',openBrowser=F){
  # Internal function 
  if(openBrowser==T){browser()}
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
