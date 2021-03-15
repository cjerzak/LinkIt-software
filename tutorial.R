
#https://github.com/cjerzak/LinkIt-software.git
options(timeout=9999999)
devtools::install_github("cjerzak/LinkIt-software/LinkIt/",
                      force = T, quiet = F,build_vignettes=F,dependencies = F)

library(data.table)
library(LinkIt)

x_mat <- data.frame("xname"=c("apple computers","j p morgan"),
                    "xdat"=c(rnorm(2)))

y_mat <- data.frame("yname"=c("apple inc","jp morgan"),
                    "ydat"=c(rnorm(2)))


z_red_LinkIt <- LinkIt(x=x_mat, y=y_mat,
                       by.x = "xname",by.y="yname",
                       fuzzy_step = T, openBrowser=F,
                       algorithm = "ml",
                        control = list(RemoveCommonWords = F,
                                       ToLower = T,
                                       NormalizeSpaces = T,
                                       RemovePunctuation = F,
                                       x.stopwordcutoff = .9,
                                       y.stopwordcutoff = .9,
                                       FuzzyThreshold = 0.10,
                                       matchMethod = "jw",
                                       qgram = 2))

z_red_fuzzy_FULL <- FastFuzzyMatch(x_red,
                                   y_red,
                                   by.x="xname", by.y= "yname",
                                   method = "jw", max_dist = 0.1,
                                   q = 2,browser=F)  