#Author: Sikder Tahsin Al Amin
#Project: incr-gamma
#Description: Compute incremental naive bayes of a data set.
#Run: Python3 filename.py dataset=file.py

import numpy as np 
import pandas as pd
from sklearn.naive_bayes import GaussianNB
from sklearn.naive_bayes import BernoulliNB
import sys
import time

sys.argv = ["incrementalNaivebayes.py","dataset=C:/My Computer/study/R/gammaPartition/test1.csv"]

if len(sys.argv) != 2:
    print("Not correct arguments. call python3 filename.py dataset=file.csv");
    sys.exit()

arg2=sys.argv[1]
arg2=arg2.split('=')
input_dataset=arg2[1] ##dataset name
print(input_dataset)

start = time.time() #time starts

trains = pd.read_csv(input_dataset, chunksize=20)

for train in trains:
    X=train.iloc[:,0:-1].to_numpy()
    y = train.iloc[:,-1].to_numpy()
    model = GaussianNB() #MultinomialNB() does not work if -ve values are present in the data set
    #model = BernoulliNB()
    model.partial_fit(X,y, np.unique(y))
    #print("coefficients each increments: ", model.class_prior_)
       
end = time.time()

print("NB")
#print("prior: ", model.class_log_prior_) #for bernoullinb()
print("Gaussian NB prior: ", model.class_prior_)
print("Total time: ",end-start)

