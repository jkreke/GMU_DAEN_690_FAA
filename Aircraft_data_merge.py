# -*- coding: utf-8 -*-
"""
Created on Thu Mar 25 12:30:55 2021

@author: belle
"""

import pandas as pd

AC_data= pd.read_csv('FAA_aircraft_list.csv', engine='python')
laser_data = pd.read_csv('laser_current.csv', engine='python')

Laser_AC_data = pd.merge(laser_data, AC_data)

Laser_AC_data.to_csv('Laser_AC_data.csv')