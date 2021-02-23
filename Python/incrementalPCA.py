#Author: Sikder Tahsin Al Amin
#Project: incr-gamma
#Description: Compute incremental PCA of a data set.
#Run: Python3 filename.py dataset=file.py

import numpy as np 
import pandas as pd
from sklearn.decomposition import IncrementalPCA
import sys
import time

sys.argv = ["incrementalPCA.py","dataset=C:/My Computer/study/R/gammaPartition/test1.csv"]

if len(sys.argv) != 2:
    print("Not correct arguments. call python3 cluster_coefficient.py dataset=tree.csv");
    sys.exit()

arg2=sys.argv[1]
arg2=arg2.split('=')
input_dataset=arg2[1] ##dataset name
print(input_dataset)

start = time.time() #time starts

trains = pd.read_csv(input_dataset, chunksize=20)

ipca = IncrementalPCA() #incremental PCA from sklearn


for train in trains:
    train = train.to_numpy() #convert data frame to numpy array
    ipca.partial_fit(train)
    new_train = ipca.transform(train)

end = time.time()


#print(ipca.components_)
print(ipca.explained_variance_)
print(ipca.explained_variance_ratio_)
print("Total time: ",end-start)

