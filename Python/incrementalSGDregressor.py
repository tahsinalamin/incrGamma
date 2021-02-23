#Author: Sikder Tahsin Al Amin
#Project: incr-gamma
#Description: Compute incremental SGDregressor of a data set.
#Run: Python3 filename.py dataset=file.py

import numpy as np 
import pandas as pd
from sklearn.linear_model import SGDRegressor
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn import linear_model
import sys
import time

sys.argv = ["incrementalPCA.py","dataset=C:/My Computer/study/R/gammaPartition/test1.csv"]

if len(sys.argv) != 2:
    print("Not correct arguments. call python3 filename.py dataset=file.csv");
    sys.exit()

arg2=sys.argv[1]
arg2=arg2.split('=')
input_dataset=arg2[1] ##dataset name
print(input_dataset)

start = time.time() #time starts

"""
train = pd.read_csv(input_dataset)
X = train.iloc[:,0:-1].to_numpy() #extract X variables and convert it to numpy array
y = train.iloc[:,-1].to_numpy() #extract y and convert and convert it to numpy array

model = make_pipeline(StandardScaler(),SGDRegressor(max_iter=1000, tol=1e-3))
model.fit(X,y)
print("coefficients: ", model.steps[1][1].coef_)
print("intercepts: ",model.steps[1][1].intercept_)
#"""
###with linearregression##
"""
data = pd.read_csv(input_dataset)
X = data.iloc[:,0:-1].to_numpy() #extract X variables and convert it to numpy array
y = data.iloc[:,-1].to_numpy() #extract y and convert and convert it to numpy array
lr = linear_model.LinearRegression()
mlr=lr.fit(X,y)
"""
#####
trains = pd.read_csv(input_dataset, chunksize=20)
#model = SGDRegressor() #incremental SGDregressor from sklearn

for train in trains:
    #sc = StandardScaler()
    #train=sc.fit_transform(train)
    #print(train)
    #X = train[:,0:-1] #extract X variables and convert it to numpy array
    #y = train[:,-1] #extract y and convert and convert it to numpy array
    #X = train.iloc[:,0:-1].to_numpy() #extract X variables and convert it to numpy array
    #y = train.iloc[:,-1].to_numpy() #extract y and convert and convert it to numpy array

    model = make_pipeline(StandardScaler(),SGDRegressor(max_iter=1000, tol=1e-3))
    X = model.named_steps['standardscaler'].fit_transform(train.iloc[:,0:-1])
    y = train.iloc[:,-1].to_numpy()
    model.named_steps['sgdregressor'].partial_fit(X,y)
    
    #new_train = model.transform(train)

end = time.time()


#print("LR")
#print("coefficients: ", mlr.coef_)
#print("intercepts: ",mlr.intercept_)
###
print("SGD")
print("coefficients: ", model.steps[1][1].coef_)
print("intercepts: ",model.steps[1][1].intercept_)
#print("coefficients: ", model.coef_)
#print("intercepts: ",model.intercept_)
print("Total time: ",end-start)

