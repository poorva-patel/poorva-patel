#!/usr/bin/env python
# coding: utf-8

# In[2]:


# import libraries and packages
import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.model_selection import cross_val_score
import pickle


# In[4]:


cancer = pd.read_csv('cancer.csv', encoding='ISO-8859-1')


# In[8]:


print(cancer.columns)


# In[12]:


features = cancer[['MedianAge', 'PctAsian', 'medIncome']]


# In[13]:


features_list = features.values.tolist()


# In[14]:


import json

# Create a sample payload
sample_payload = json.dumps({"features": features_list[0]})  
print(sample_payload)


# In[ ]:




