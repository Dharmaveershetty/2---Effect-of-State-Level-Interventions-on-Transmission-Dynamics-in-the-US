# -*- coding: utf-8 -*-
"""
Created on Sat Nov 21 14:56:36 2020

@author: mcatillon
"""


import csv
import os
from datetime import datetime
import sys
import time
import pandas as pd
import numpy as np
from tqdm import tqdm
import matplotlib as plt
import glob


data_folder = r'C:\Users\mcatillon\Dropbox\1-Harvard\1-Research\Covid_19_group\Data\Google_Trends'
crosswalk_folder = r'C:\Users\mcatillon\Dropbox\1-Harvard\1-Research\Covid_19_group\Data\dma_county_crosswalk'

# Load clean crosswalk
# Source https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/IVXEHT/DBOGRS&version=7.4
crosswalk = pd.read_csv(os.path.join(crosswalk_folder, 'DMA-zip.csv'))

# Load hygiene files
all_trends = glob.glob(os.path.join(data_folder, "*.csv"))
df_from_each = (pd.read_csv(f) for f in all_trends)
trends = pd.concat(df_from_each, ignore_index=True)

# Describe hygiene
trends.describe()

# remove first column
trends = trends.iloc[:,1:]

# rename columns
trends.columns = ['date','awareness','hygiene', 'dma']

# Replace zero values by missing
trends.replace(0, np.nan, inplace=True)

# Calculate average values for awareness and hygiene for each date dma
mean_trends = trends.groupby(['date','dma']).mean().reset_index()

# Merge dataset with crosswalk
crosswalk = crosswalk.rename(columns={'DMA CODE': 'dma'})
crosswalk = crosswalk.drop(columns='ZIPCODE')
crosswalk.drop_duplicates(inplace=True)
trends_county = mean_trends.merge(crosswalk, on='dma')

# Export final dataset to csv
trends_county.to_csv('trends_county.csv', index=False)
