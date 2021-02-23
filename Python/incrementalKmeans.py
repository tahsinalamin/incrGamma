#Author: Sikder Tahsin Al Amin
#Project: incr-gamma
#Description: Compute incremental kmeans of a data set.
#Run: Python3 filename.py dataset=file.py

import numpy as np 
import pandas as pd
from sklearn.cluster import MiniBatchKMeans
import sys
import time

#sys.argv = ["incrementalKmeans.py","dataset=C:/My Computer/study/R/gammaPartition/test1.csv"]

if len(sys.argv) != 2:
    print("Not correct arguments. call python3 filename.py dataset=file.csv");
    sys.exit()

arg2=sys.argv[1]
arg2=arg2.split('=')
input_dataset=arg2[1] ##dataset name
print(input_dataset)

start = time.time() #time starts

trains = pd.read_csv(input_dataset, chunksize=1000)

for train in trains:
    X=train.iloc[:,0:-1].to_numpy()
    y = train.iloc[:,-1].to_numpy()
    model = MiniBatchKMeans(n_clusters=2,random_state=0)
    model.partial_fit(X)
    #print("coefficients each increments: ", model.class_prior_)
       
end = time.time()

print("KM")
#print("prior: ", model.class_log_prior_) #for bernoullinb()
print("Clusters centers: ", model.cluster_centers_)
print("Total time: ",end-start)

