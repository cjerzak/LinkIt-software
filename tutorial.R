
#https://github.com/cjerzak/LinkIt-software.git
options(timeout=9999999); devtools::install_github("cjerzak/LinkIt-software/LinkIt/",
                      force = F, quiet = F,build_vignettes=F,dependencies = T)
library(LinkIt)
library(data.table)

x_mat <- data.frame("xname"=c("apple computers","j p morgan"),
                    "xdat"=c(rnorm(2)))

y_mat <- data.frame("yname"=c("apple inc","jp morgan"),
                    "ydat"=c(rnorm(2)))

fuzzyThres <- 0.2

z_LinkIt_markov <- LinkIt(x=as.data.table(x_mat), y=as.data.table(y_mat),
                          by.x = "xname",by.y="yname",
                          openBrowser=F,
                          algorithm = "markov", returnDiagnostics = T,
                          control = list(RemoveCommonWords = F,
                                         ToLower = T,
                                         NormalizeSpaces = T,
                                         RemovePunctuation = F,
                                         FuzzyThreshold = fuzzyThres,
                                         matchMethod = "jw",
                                         qgram = 2))

z_LinkIt_bipartite <- LinkIt(x=as.data.table(x_mat), y=as.data.table(y_mat),
                              by.x = "xname",by.y="yname",
                             algorithm = "bipartite", openBrowser=F,
                              returnDiagnostics = T,
                              control = list(RemoveCommonWords = F,
                                             ToLower = T,
                                             NormalizeSpaces = T,
                                             RemovePunctuation = F,
                                             FuzzyThreshold = fuzzyThres,
                                             matchMethod = "jw",
                                             qgram = 2))

#for machine learning clustering: 
#must do install.packages("reticulate")
#from terminal: pip install tensorflow 
#pip install keras
#pip install chars2vec
#make sure pip is using same python version as recitulate 
z_LinkIt_ml <- LinkIt(x=as.data.table(x_mat), y=as.data.table(y_mat),
                       by.x = "xname",by.y="yname",
                      algorithm = "ml", openBrowser=F, returnDiagnostics = T,
                        control = list(RemoveCommonWords = F,
                                       ToLower = T,
                                       NormalizeSpaces = T,
                                       RemovePunctuation = F,
                                       FuzzyThreshold = fuzzyThres,
                                       matchMethod = "jw",
                                       qgram = 2))

z_fuzzy     <-      FastFuzzyMatch(x_mat,y_mat,
                                   by.x="xname", by.y= "yname",
                                   method = "jw", max_dist = fuzzyThres,
                                   q = 2,openBrowser=F)  

z_LinkIt_markov
z_LinkIt_bipartite
z_LinkIt_ml
z_fuzzy






