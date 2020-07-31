# -*- coding: utf-8 -*-
"""
ARIMAX AND SARIMAX

@author: Admin
"""
import pandas as pd
import os
from statsmodels.tsa.statespace.sarimax import SARIMAX
os.chdir(r'C:\Users\Admin\Desktop\Data science stuff\ARIMA-And-Seasonal-ARIMA-master')
df=pd.read_csv('sales.csv')
df.head()
df.index=df.Month
df.plot()
#creating a model
help(SARIMAX)
arimax_model=SARIMAX(df['Perrin Freres monthly champagne sales millions 64-72'],order=(1,1,1),exog=df['inflation'])
sarimax_model=SARIMAX(df['Perrin Freres monthly champagne sales millions 64-72'],order=(1,1,1),seasonal_order=(1,1,1,4),exog=df['inflation'])
res=sarimax_model.fit(disp=False)
res.summary
arimax_model=SARIMAX(df['Perrin Freres monthly champagne sales millions 64-72'],order=(1,1,1),exog=df['inflation'])
