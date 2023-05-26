# import packages
library(dplyr)
library(tidyverse)
library(RSQLite)
library(lubridate)
library(lmtest)
library(multiwayvcov)
library(fixest)
library(plm)
library(sandwich)
library(lfe)
library(stargazer)
library(data.table)
library(robustHD)
library(pacman)
library(DescTools)

#convert
d$ppent<-d$ppent/1000
d$avg_TDC1<-d$avg_TDC1/1000


#Data imputation on the original dataset where all R&D are kept
## Replace NA in R&D with year mean
d <- d %>% 
  group_by(fyear) %>% 
  mutate(xrd = ifelse(is.na(xrd), 
                      mean(xrd, na.rm=TRUE), xrd))
d <- d %>% 
  group_by(fyear) %>% 
  mutate(R_D = ifelse(is.na(R_D), 
                       mean(R_D, na.rm=TRUE), R_D))

d <- d %>% 
  group_by(fyear) %>% 
  mutate(Liquidity_lagged = ifelse(is.na(Liquidity_lagged), 
                      mean(Liquidity_lagged, na.rm=TRUE),Liquidity_lagged))

##Create a dummy have_fem to indicate if there is female on board
d$have_fem<-ifelse(d$pct_female==0,0,1)


##Winsorize
d$at<-Winsorize(d$at,probs = c(0.01, 0.99),na.rm = TRUE)
d$log_sale<-Winsorize(d$log_sale,probs = c(0.01, 0.99),na.rm = TRUE)
d$M_B<-Winsorize(d$M_B,probs = c(0.01, 0.99),na.rm = TRUE)
d$ppent<-Winsorize(d$ppent,probs = c(0.01, 0.99),na.rm = TRUE)
d$avg_TDC1<-Winsorize(d$avg_TDC1,probs = c(0.01, 0.99),na.rm = TRUE)
d$Liquidity_lagged<-Winsorize(d$Liquidity_lagged,probs = c(0.01, 0.99),na.rm = TRUE)
d$avg_age<-Winsorize(d$avg_age,probs = c(0.01, 0.99),na.rm = TRUE)
d$R_D<-Winsorize(d$R_D,probs = c(0.01, 0.99),na.rm = TRUE)
d$Leverage<-Winsorize(d$Leverage,probs = c(0.01, 0.99),na.rm = TRUE)
d$CashHolding<-Winsorize(d$CashHolding,probs = c(0.01, 0.99),na.rm = TRUE)
d$prccd_var<-Winsorize(d$prccd_var,probs = c(0.01, 0.99),na.rm = TRUE)
d$ROE<-Winsorize(d$ROE,probs = c(0.01, 0.99),na.rm = TRUE)

##Create a subset drop_xrd where we only keep R&D with values
d$xrd_na<-ifelse(is.na(d$xrd),1,0)
drop_xrd<-subset(d,xrd_na==0)

f2015<-subset(d,fyear=2015)



### Clustered standard errors
## 1. R&D

#Have female, clusterd by company and year
reg1 = felm(R_D~post_covid*have_fem+log(at)+log_sale+M_B+ppent+avg_TDC1+Liquidity_lagged+
              avg_age+ROE+factor(fyear) | industry+politics| 0 | gvkey+fyear, data = drop_xrd)

summary(reg1)



#Have female, clusterd by industry and year
reg2 = felm(R_D~post_covid*have_fem+log(at)+log_sale+M_B+ppent+avg_TDC1+Liquidity_lagged+
              avg_age+ROE+factor(fyear) | industry+politics| 0 | industry+fyear, data = drop_xrd)

summary(reg2)

#Female percentage, clusterd by company and year
reg3 = felm(R_D~post_covid*pct_female+log(at)+log_sale+M_B+ppent+avg_TDC1+Liquidity_lagged+
              avg_age+ROE+factor(fyear) | industry+politics | 0 | gvkey+fyear, data = drop_xrd)

summary(reg3)


##Female percentage, clusterd by industry and year
reg4 = felm(R_D~post_covid*pct_female+log(at)+log_sale+M_B+ppent+avg_TDC1+Liquidity_lagged+
              avg_age+ROE +factor(fyear)| industry+politics| 0 | industry+fyear, data = drop_xrd)

summary(reg4)

#generate tex table, need to change directory if running on your device
stargazer(reg1,reg2,reg3,reg4,type='html',
          title = "R&D Expenditure",ci=FALSE, no.space = TRUE, 
          header = FALSE,
          add.lines=list(c("Industry FE", "Yes", "Yes","Yes","Yes"), 
                         c("Year FE", "Yes", "Yes","Yes","Yes")),c("Politics FE", "Yes", "Yes","Yes","Yes"), c("Clustered SE", "Company + Year", "Industry + Year","Company + Year","Industry + Year"),out ="/Users/skylermacbook/Desktop/DS Capstone/table/RD.tex")


## 2. Risk-taking
# a. Leverage

#Have female, clusterd by company and year
reg5 = felm(Leverage~post_covid*have_fem+log(at)+log_sale+M_B+ppent+avg_TDC1+Liquidity_lagged+
              avg_age+ROE +factor(fyear)| industry+politics | 0 | gvkey+fyear, data = d)

summary(reg5)

#Have female, clusterd by industry and year
reg6 = felm(Leverage~post_covid*have_fem+log(at)+log_sale+M_B+ppent+avg_TDC1+Liquidity_lagged+
              avg_age+ROE +factor(fyear)| industry+politics | 0 | industry+fyear, data = d)

summary(reg6)

#Female percentage, clusterd by company and year
reg7 = felm(Leverage~post_covid*pct_female+log(at)+log_sale+M_B+ppent+avg_TDC1+Liquidity_lagged+
              avg_age+ROE+factor(fyear) | industry+politics| 0 | gvkey+fyear, data = d)

summary(reg7)


##Female percentage, clusterd by industry and year
reg8 = felm(Leverage~post_covid*pct_female+log(at)+log_sale+M_B+ppent+avg_TDC1+Liquidity_lagged+
              avg_age+ROE +factor(fyear)| industry+politics | 0 | industry+fyear, data = d)

summary(reg8)

#generate tex table, need to change directory if running on your device
stargazer(reg5,reg6,reg7,reg8,type='html',
          title = "Leverage",ci=FALSE, no.space = TRUE, 
          header = FALSE,
          add.lines=list(c("Industry FE", "Yes", "Yes","Yes","Yes"), 
                         c("Year FE", "Yes", "Yes","Yes","Yes")),
                         c("Politics FE", "Yes", "Yes","Yes","Yes"), 
                         c("Clustered SE", "Company + Year", "Industry + Year","Company + Year","Industry + Year"),out = "/Users/skylermacbook/Desktop/DS Capstone/table/Lev.tex")

# b. stock volatility

#Have female, clusterd by company and year
reg9 = felm(prccd_var~post_covid*have_fem+log(at)+log_sale+M_B+ppent+avg_TDC1+Liquidity_lagged+
              avg_age+ROE+factor(fyear) | industry+politics | 0 | gvkey+fyear, data = d)

summary(reg9)

#Have female, clusterd by industry and year
reg10 = felm(prccd_var~post_covid*have_fem+log(at)+log_sale+M_B+ppent+avg_TDC1+Liquidity_lagged+
              avg_age+ROE+factor(fyear) | industry+politics | 0 | industry+fyear, data = d)

summary(reg10)

#Female percentage, clusterd by company and year
reg11 = felm(prccd_var~post_covid*pct_female+log(at)+log_sale+M_B+ppent+avg_TDC1+Liquidity_lagged+
              avg_age+ROE+factor(fyear) | industry+politics| 0 | gvkey+fyear, data = d)

summary(reg11)


##Female percentage, clusterd by industry and year
reg12 = felm(prccd_var~post_covid*pct_female+log(at)+log_sale+M_B+ppent+avg_TDC1+Liquidity_lagged+
              avg_age+ROE+factor(fyear) | industry+politics | 0 | industry+fyear, data = d)

summary(reg12)

#generate tex table, need to change directory if running on your device
stargazer(reg9,reg10,reg11,reg12,type='html',
          title = "Stock price volatility",ci=FALSE, no.space = TRUE, 
          header = FALSE,
          add.lines=list(c("Industry FE", "Yes", "Yes","Yes","Yes"), 
                         c("Year FE", "Yes", "Yes","Yes","Yes")),
                         c("Politics FE", "Yes", "Yes","Yes","Yes"), 
                         c("Clustered SE", "Company + Year", "Industry + Year","Company + Year","Industry + Year"),out = "/Users/skylermacbook/Desktop/DS Capstone/table/vol.tex")

## 3. Cash holdings
#Have female, clusterd by company and year
reg13 = felm(CashHolding~post_covid*have_fem+log(at)+log_sale+M_B+ppent+avg_TDC1+Liquidity_lagged+
              avg_age+ROE+factor(fyear) | industry+politics | 0 | gvkey+fyear, data = d)

summary(reg13)

#Have female, clusterd by industry and year
reg14 = felm(CashHolding~post_covid*have_fem+log(at)+log_sale+M_B+ppent+avg_TDC1+Liquidity_lagged+
               avg_age+ROE+factor(fyear) | industry+politics | 0 | industry+fyear, data = d)

summary(reg14)

#Female percentage, clusterd by company and year
reg15 = felm(CashHolding~post_covid*pct_female+log(at)+log_sale+M_B+ppent+avg_TDC1+Liquidity_lagged+
               avg_age+ROE +factor(fyear)| industry+politics | 0 | gvkey+fyear, data = d)

summary(reg15)


##Female percentage, clusterd by industry and year
reg16 = felm(CashHolding~post_covid*pct_female+log(at)+log_sale+M_B+ppent+avg_TDC1+Liquidity_lagged+
               avg_age+ROE+factor(fyear)| industry+politics | 0 | industry+fyear, data = d)

summary(reg16)

#generate tex table, need to change directory if running on your device
stargazer(reg13,reg14,reg15,reg16,type='html',
          title = "Cash Holdings",ci=FALSE, no.space = TRUE, 
          header = FALSE,
          add.lines=list(c("Industry FE", "Yes", "Yes","Yes","Yes"), 
                         c("Year FE", "Yes", "Yes","Yes","Yes")),
          c("Politics FE", "Yes", "Yes","Yes","Yes"), 
          c("Clustered SE", "Company + Year", "Industry + Year","Company + Year","Industry + Year"),out = "/Users/skylermacbook/Desktop/DS Capstone/table/cash.tex")
summary(d)