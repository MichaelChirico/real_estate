#Michael Chirico
#July 3, 2015
#
#This file is designed to scrape the DoR Website for up-to-date information
#  about all properties in Round 2 of the Philadelphia Tax Collection Experiment

from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from bs4 import BeautifulSoup
from time import sleep
import os
import csv
import datetime
import pandas as pd
import random

#set correct directory
os.chdir('/media/data_drive/real_estate/round_2_scraping')

#access website through Firefox
driver = webdriver.Firefox()

#read in OPA numbers from final data file
opas = pd.read_csv('/home/michael/Desktop/research/Sieg_LMI_Real_Estate_Delinquency/round_two/round_2_full_data.csv',
                   usecols=['opa_no'],dtype={'opa_no':str})['opa_no']

with open('round_2_balances_'+datetime.date.today().strftime('%y%m%d')+'.csv','w') as csvf:
    #send output to dated .csv
    csvfile=csv.writer(csvf)
    csvfile.writerow(['opa_no','principal','interest','penalty','other_due','total_due','accurate_date'])
    for opa in opas:
        #initialize page, make sure we're where we think we are
        driver.get('http://www.phila.gov/revenue/RealEstateTax/default.aspx')
        assert "Revenue Department" in driver.title

        #"type in" OPA number, "click" enter button
        driver.find_element_by_id("ctl00_BodyContentPlaceHolder_SearchByBRTControl_txtTaxInfo").send_keys(opa)
        driver.find_element_by_id("ctl00_BodyContentPlaceHolder_SearchByBRTControl_btnTaxByBRT").click()

        #parse result
        soup = BeautifulSoup(driver.page_source)

        #extract tax info from last row of table
        principal, interest, penalty, other, total = \
            [c.text for c in soup.find('tr',{'style':'color:Black;background-color:#CCCCCC;font-weight:bold;'}).findAll('td')[1:6]]

        #extract date: "Includes Payments Through"
        accurate_date = soup.find('span',{'id':'ctl00_BodyContentPlaceHolder_GetTaxInfoControl_frm_lblPaymentPostDate'}).text
        
        csvfile.writerow([opa,principal,interest,penalty,other,total,accurate_date])
        
        #Stop for a random period of time to throw off bot detectors
        sleep(3*random.random())
        
