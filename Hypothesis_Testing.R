#################################################################################################
## OZKAN EMRE OZDEMIR                                                                           #
## HOMEWORK 3 : Hypothesis Testing                                                              #
## 04/21/16                                                                                     #
## Class:  Methods for Data Analysis                                                            #
##                                                                                              #
##  http://www.fsa.usda.gov/FSA/webapp?area=newsroom&subject=landing&topic=foi-er-fri-pfi       #
##                                                                                              #
##   Need:                                                                                      #
##                                                                                              #
##    -2011 Farm Payment File (27MB) txt file                                                   #
##    -State and County Code List (374KB) xls file (probably convert this to csv)               #
#################################################################################################
## Clear objects from Memory :
rm(list=ls())
##Clear Console:
cat("\014")
##
##Set working directory- :
setwd('~/DataAnalysis/3_Outliers_MissingData_Hypothesis')
##----Import Libraries-----
library(plyr)
#########################################----Hypotheses to test-----#############################
#                                                                                               #
#  Test these two things:                                                                       #
#                                                                                               #
#    1.  Does our sample equally represent all 50 states?                                       #
#                                                                                               #
#    2.  Does our sample equally represent all 50 states, weighted by number of farms/state?    #
#                                                                                               #
#     Note- you can find the farms per state in census data.                                    #
#                                                                                               #
#################################################################################################

##-----Read in the data-----
data = read.csv("CAS.WDC11019.PMT11.FINAL.DT11186.TXT", sep=";",
                header=FALSE, stringsAsFactors=FALSE)

##----Trim Whitespaces-----
trim = function (x) gsub("^\\s+|\\s+$", "", x)
data = as.data.frame(apply(data,2,trim), stringsAsFactors=FALSE)

names(data) = c('state_code', 'county_code', 'cust_num', 'program_code', 'program_year',
                'commodity_code', 'amount', 'date', 'cat_code', 'farm_num',
                'calendar_year', 'fiscal_year', 'seq_num')

##------Read State/County File-----
county_state_codes = read.csv("foia_state_county_codes-1.csv", stringsAsFactors=FALSE)
county_state_codes$state_code = county_state_codes$Stcd
county_state_codes$Stcd = NULL
county_state_codes$county_code = county_state_codes$Cntycd
county_state_codes$Cntycd = NULL
        
##----Merge files together----
data = merge(data, county_state_codes, by=c("state_code", "county_code"), all.x=TRUE)
        
##-----Probably do some data exploration----
head(data)
## number of dollars each state recevived 
How_much_each_state_received = aggregate(x = as.numeric(gsub(",","",data$amount)), by =list(State = data$ST), FUN = sum)
How_much_each_state_received =rename(How_much_each_state_received ,c("x" = "Total_Amount"))
tail(How_much_each_state_received[order(How_much_each_state_received$Total_Amount),])

##------------ Let's answer the 1st question--------------
##   1.  Does our sample equally represent all 50 states?
##
##----Perform a test for equal representation-----------
##For categorical comparison tests,
## If the sample/subgroup size is large enough,
##  Use Chi-squared test
chi_data = data.frame('occurrence'= How_much_each_state_received$Total_Amount,
                      'expected_per'= rep(1/50,50))
chisq.test(chi_data$occurrence, p = chi_data$expected_per)

# p-value < 2.2e-16, very low p value! and X-squared = 2177400000
# Question 1.  Does our sample equally represent all 50 states?
# As the test confirms, the answer is No, it doesnt!
#

##------------ Let's answer the 2nd  question--------------
#    2.  Does our sample equally represent all 50 states, weighted by number of farms/state?   
#
##----Access the farms/state data-----
farm_data =  read.csv('FarmsPerState.csv', stringsAsFactors = FALSE)

##----Derive the weights for each state----
w = farm_data$Farms/sum(farm_data$Farms)

##----Perform a test for equal repreentation by farms/state-----
chi_farms_per_state = data.frame('occurrence'= farm_data$Farms,
                                 'expected_per'= w)
chisq.test(chi_farms_per_state$occurrence, p = chi_data$expected_per)

## p-value < 2.2e-16, again very low p value 
# Question 2.  Does our sample equally represent all 50 states, weighted by number of farms/state?
# As the test confirms, the answer is No, it doesnt!

################################################################################################
##                                                    End                                      #
################################################################################################