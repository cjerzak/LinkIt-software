# LinkIt

An R package 

## Installation

The most recent version of `LinkOrgs` can be installed directly from the repository using the `devtools` package

```
devtools::install_github("cjerzak/LinkOrgs-software/LinkOrgs")
```

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
linkedOrgs <- LinkOrgs(x  = x, 
                        y =  y, 
                        by.x = "orgnames_x", 
                        by.y = "orgnames_y"
                        MaxDist = 0.4, 
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
                                             z = z, 
                                             z_true = z_true)
``` 


## License

Creative Commons Attribution-Noncommercial-No Derivative Works 4.0, for academic use only.

## Acknowledgments
Thank you to Gary King, Kosuke Imai, Xiang Zhou, and members of the Imai Research Workshop for valuable feedback on this research project. 

>>>>>>> 95a13a4ea40c90b623d7aa3a074611a11551abc1
