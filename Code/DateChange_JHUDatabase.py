# -*- coding: utf-8 -*-
"""

@author: dharma

"""

import csv
import datetime as dt
import sys
import time
import pandas as pd
import numpy as np
from tqdm import tqdm
import matplotlib as plt


df = pd.read_csv ("/Users/simba/Desktop/NPI_County_JHU.csv")

for col in ['stay at home', 
            '>50 gatherings',
            '>500 gatherings',
            'public schools',
            'restaurant dine-in',
            'entertainment/gym',
            'federal guidelines',
            'foreign travel ban',
            'stay at home rollback',
            '>50 gatherings rollback',
            '>500 gatherings rollback',
            'restaurant dine-in rollback',
            'entertainment/gym rollback']:
    df[col] = df[col].astype('Int64')
df.dtypes
 
df['stay at home'] = df['stay at home'].fillna(1).astype('Int64')
df['>50 gatherings'] = df['>50 gatherings'].fillna(1).astype('Int64')
df['>500 gatherings'] = df['>500 gatherings'].fillna(1).astype('Int64')
df['public schools'] = df['public schools'].fillna(1).astype('Int64')
df['restaurant dine-in'] = df['restaurant dine-in'].fillna(1).astype('Int64')
df['entertainment/gym'] = df['entertainment/gym'].fillna(1).astype('Int64')
df['federal guidelines'] = df['federal guidelines'].fillna(1).astype('Int64')
df['foreign travel ban'] = df['foreign travel ban'].fillna(1).astype('Int64')
df['stay at home rollback'] = df['stay at home rollback'].fillna(1).astype('Int64')
df['>50 gatherings rollback'] = df['>50 gatherings rollback'].fillna(1).astype('Int64')
df['>500 gatherings rollback'] = df['>500 gatherings rollback'].fillna(1).astype('Int64')
df['restaurant dine-in rollback'] = df['restaurant dine-in rollback'].fillna(1).astype('Int64')
df['entertainment/gym rollback'] = df['entertainment/gym rollback'].fillna(1).astype('Int64')

for col in ['stay at home', 
            '>50 gatherings',
            '>500 gatherings',
            'public schools',
            'restaurant dine-in',
            'entertainment/gym',
            'federal guidelines',
            'foreign travel ban',
            'stay at home rollback',
            '>50 gatherings rollback',
            '>500 gatherings rollback',
            'restaurant dine-in rollback',
            'entertainment/gym rollback']:
    df[col] = df[col].apply(dt.datetime.fromordinal)
df.to_csv("/Users/simba/Desktop/NPI_County_JHU.csv")


