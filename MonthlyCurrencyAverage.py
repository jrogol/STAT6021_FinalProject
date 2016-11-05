import pandas as pd
import numpy as np
import time
import re
from bs4 import BeautifulSoup
import urllib.request
import os

os.chdir('/Users/jamesrogol/Box Sync/Fall 2016/STAT 6021/Final Project/STAT6021_FinalProject')

# Read in the Currency values
data = pd.read_csv('Currencies.csv', header=None, delimiter=',,')

# The abbrviations are all we care about - remove NaN values and duplicates.
currencies = data[1].dropna().unique()
years = range(2006,2017)

AllData = pd.DataFrame(columns=['currency','rate','month','year','days'])

for j in currencies:
    for year in years:
        try:
            url = 'http://www.x-rates.com/average/?from=%s&to=GBP&amount=1&year=%s' % (j, year)
            html = urllib.request.urlopen(url)
            soup = BeautifulSoup(html,"html5lib")
            rates = soup.find('ul', attrs={'class':'OutputLinksAvg'}).findChildren('span',
                attrs={'class':'avgRate'})
            months = soup.find('ul', attrs={'class':'OutputLinksAvg'}).findChildren('span',
                attrs={'class':'avgMonth'})
            days = soup.find('ul', attrs={'class':'OutputLinksAvg'}).findChildren('span',
                attrs={'class':'avgDays'})
            for i,_ in enumerate(rates):
                rate = rates[i].contents
                month = months[i].contents
                day = re.sub('[^0-9]+','',days[i].contents[0])
                temp = pd.DataFrame({'currency':j,
                                    'rate':rate,
                                    'month':month,
                                    'year':year,
                                    'days': day})
                AllData = pd.concat([AllData, temp], axis=0)
            time.sleep(10+np.random.uniform(-5,5))
        except:
            print('Error in %s, %s' % (j, year))
        print(j,year)
        
    time.sleep(10+np.random.uniform(-10,10))
    
    
AllData.to_csv('ExchangeRates.csv', index=False)
