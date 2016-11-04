import pandas as pd
import numpy as np
import time
import re
from bs4 import BeautifulSoup
import urllib.request

years = range(2006,2017)
currencies = {'Belgium':'EUR','Russia':'RUB'}


# html = urllib.request.urlopen("http://www.x-rates.com/average/?from=USD&to=GBP&amount=1&year=2007")
# soup = BeautifulSoup(html,"html5lib")

AllData = pd.DataFrame(columns=['currency','rate','month','year','days'])

for j in currencies:
    for year in years:
        try:
            url = 'http://www.x-rates.com/average/?from=%s&to=GBP&amount=1&year=%s' % (currencies[j], year)
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
                temp = pd.DataFrame({'currency':currencies[j],
                                    'rate':rate,
                                    'month':month,
                                    'year':year,
                                    'days': day})
                AllData = pd.concat([AllData, temp], axis=0)
            time.sleep(10+np.random.uniform(-5,5))
        except:
            print('Error in %s, %s' % (currencies[j], year))
        print(currencies[j],year)
        
    time.sleep(10+np.random.uniform(-10,10))
    
    

