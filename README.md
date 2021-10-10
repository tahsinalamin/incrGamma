# incrGamma
Incremental computation of machine learning models with Gamma summarization  matrix. The theory is in http://www2.cs.uh.edu/~dsss/pdf/j_incrgamma.pdf

## HOW TO COMPILE ##
**Command to build the package**

- R CMD build incrGamma

**Command install the package**

- R CMD INSTALL build_package_name


## HOW TO RUN ##
**Load and use the package in R prompt**

library('incrGamma')

LR('test1.csv',sparse=N)

LRnPCA('test1.csv',sparse=Y)

NB.gamma('test1.csv')

kmeans.gamma('test1.csv',k=3)

## Required R packages ##
 - Rcpp
 - chunkR
 - e1071
 - pracma
 - data.table
