# incrGamma
Incremental computation of machine learning models with Gamma summarization  matrix. The theory is in http://www2.cs.uh.edu/~dsss/pdf/j_incrgamma.pdf

**build the package**

R CMD build incrGamma

**install the package**

R CMD INSTALL build_package_name

**Load and use the package in R prompt**

library('incrGamma')

LRnPCA('test1.csv',sparse=Y)

NB.gamma('test1.csv')

kmeans.gamma('test1.csv',k=3)

