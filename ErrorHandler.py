import pandas as pd
import numpy as np
import time
import re
from bs4 import BeautifulSoup
import urllib.request
import os

# Read in the log file of the errors, splitting on a newline.
Errors = open('ScraperErrors.txt').read()
Errors = Errors.split('\n')

# Extract just the lines that begin with 'Error'
All_Errors = [Errors[i] for i in range(len(Errors)) if Errors[i].startswith('Error')]
All_Errors = [All_Errors[i].replace('Error in ', '') for i in range(len(All_Errors))]


# Append to Data Frame: http://stackoverflow.com/questions/17530542/how-to-add-pandas-data-to-an-existing-csv-file

for j in All_Errors:
    AllData = pd.DataFrame(columns=['currency','rate','month','year','days'])
    country = j.split(',')[0]
    year = j.split(',')[1].strip()
    x =2
    while x != 1:
        try:
            url = 'http://www.x-rates.com/average/?from=%s&to=GBP&amount=1&year=%s' % (country, year)
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
                temp = pd.DataFrame({'currency':country,
                                    'rate':rate,
                                    'month':month,
                                    'year':year,
                                    'days': day})
                AllData = pd.concat([AllData, temp], axis=0)
            AllData.to_csv('ExchangeRates.csv', mode='a', index=False, header=False)
            x = 1
            print(j)
            time.sleep(10+np.random.uniform(-5,5))
        except:
            print('Error in %s, Retrying' % (j))
            time.sleep(10+np.random.uniform(-5,5))
            x = 2