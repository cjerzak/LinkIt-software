
#https://github.com/cjerzak/LinkIt-software.git
options(timeout=9999999); devtools::install_github("cjerzak/LinkIt-software/LinkIt/",
                      force = T, quiet = F,build_vignettes=F,dependencies = T)
library(LinkIt)
library(data.table)

x_mat <- data.frame("xname"=c("apple computers","j p morgan"),
                    "xdat"=c(rnorm(2)))

y_mat <- data.frame("yname"=c("apple inc","jp morgan"),
                    "ydat"=c(rnorm(2)))

z_red_LinkIt_markov <- LinkIt(x=as.data.table(x_mat), y=as.data.table(y_mat),
                          by.x = "xname",by.y="yname",
                          openBrowser=F,
                          algorithm = "markov", returnDiagnostics = T,
                          control = list(RemoveCommonWords = F,
                                         ToLower = T,
                                         NormalizeSpaces = T,
                                         RemovePunctuation = F,
                                         FuzzyThreshold = 0.10,
                                         matchMethod = "jw",
                                         qgram = 2))


z_red_LinkIt_bipartite <- LinkIt(x=as.data.table(x_mat), y=as.data.table(y_mat),
                              by.x = "xname",by.y="yname",
                              fuzzy_step = T, openBrowser=F,
                              algorithm = "markov", returnDiagnostics = T,
                              control = list(RemoveCommonWords = F,
                                             ToLower = T,
                                             NormalizeSpaces = T,
                                             RemovePunctuation = F,
                                             FuzzyThreshold = 0.10,
                                             matchMethod = "jw",
                                             qgram = 2))

z_red_LinkIt_ml <- LinkIt(x=as.data.table(x_mat), y=as.data.table(y_mat),
                       by.x = "xname",by.y="yname",
                       fuzzy_step = T, openBrowser=F,
                       algorithm = "bipartite", returnDiagnostics = T,
                        control = list(RemoveCommonWords = F,
                                       ToLower = T,
                                       NormalizeSpaces = T,
                                       RemovePunctuation = F,
                                       FuzzyThreshold = 0.10,
                                       matchMethod = "jw",
                                       qgram = 2))

z_red_fuzzy_FULL <- FastFuzzyMatch(x_red,
                                   y_red,
                                   by.x="xname", by.y= "yname",
                                   method = "jw", max_dist = 0.1,
                                   q = 2,browser=F)  