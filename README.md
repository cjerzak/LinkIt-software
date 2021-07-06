# LinkOrgs

An R package for linking datasets on organizations using the approach in Jerzak & Libgober (2021).  

## Installation

The most recent version of `LinkOrgs` can be installed directly from the repository using the `devtools` package

```
devtools::install_github("cjerzak/LinkOrgs-software/LinkOrgs")
```

The machine-learning based algorithm accessible via the `algorithm="ml"` option relies on the `chars2vec` `Python` package. For details about downloading, see `https://github.com/IntuitionEngineeringTeam/chars2vec`. Tensorflow and Keras are dependencies. The network-based linkage approaches (`algorithm="bipartite"` and `algorithm = "markov"`) do not require these packages. 

Note that all options require Internet access in order to download the LinkedIn-based network information. 

## Walkthrough

After installing the package, let's get some experience with it in a simple example. 


```
# load in package 
library(LinkOrgs)

# set up synthetic data for the merge 
x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds")
x <- data.frame("orgnames_x"=x_orgnames)
y <- data.frame("orgnames_y"=y_orgnames)
```
After creating these synthetic datasets, we're now ready to merge them! 

``` 
# perform merge 
z_linked <- LinkOrgs(x  = x, 
                     y =  y, 
                     by.x = "orgnames_x", 
                     by.y = "orgnames_y",
                     MaxDist = 0.6, 
                     algorithm = "bipartite", 
                     DistanceMeasure = "jaccard")
```

Using the package, we can also assess performance against a ground-truth merged dataset (if available). 
``` 
# (After running the above code)
z_true <- data.frame("orgnames_x"=x_orgnames, "orgnames_y"=y_orgnames)

# Get performance matrix 
PerformanceMatrix <- AssessMatchPerformance(x  = x, 
                                            y =  y, 
                                            by.x = "orgnames_x", 
                                            by.y = "orgnames_y", 
                                            z = z_linked, 
                                            z_true = z_true)
``` 


## License

Creative Commons Attribution-Noncommercial-No Derivative Works 4.0, for academic use only.

## Acknowledgments
Thank you to Gary King, Kosuke Imai, Xiang Zhou, and members of the Imai Research Workshop for valuable feedback on this research project. We also would like to thank Gil Tamir for invaluable research assistance. 

>>>>>>> 95a13a4ea40c90b623d7aa3a074611a11551abc1
