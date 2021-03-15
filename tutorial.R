
#https://github.com/cjerzak/LinkIt-software.git
devtools::install_github("cjerzak/LinkIt-software/LinkIt/",
                      force = T, quiet = F,build_vignettes=F,dependencies = F)

library(LinkIt)

x_mat <- data.frame("xname"=c("apple computers","j p morgan"),
                    "xdat"=c(rnorm(2)))

y_mat <- data.frame("yname"=c("apple inc","jp morgan"),
                    "ydat"=c(rnorm(2)))


z_red_LinkIt <- LinkIt(x=x_mat, y=y_mat,
                       by.x = "xname",by.y="yname",
                       fuzzy_step = T,algorithm = "ml",
                        control = list(RemoveCommonWords = F,
                                       ToLower = T,
                                       NormalizeSpaces = T,
                                       RemovePunctuation = F,
                                       x.stopwordcutoff = .9,
                                       y.stopwordcutoff = .9,
                                       FuzzyThreshold = 0.10,
                                       matchMethod = "", #must be clust
                                       qgram = 2),
                        browser= T)


z_red_fuzzy_FULL <- FastFuzzyMatch(x_red,y_red,
                                   by.x=by.x, by.y=by.y,
                                   method = method_, max_dist = max(dist_seq),
                                   q = qgram,browser=F)  