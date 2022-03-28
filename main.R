setwd("~/2022 Spring/PREDICTIVE ANALYTICS/dmba-datasets/dmba")
library(e1071)
library(dplyr)
library(tidyverse)
library(tidyr)


bank.df <- read.csv("UniversalBank.csv")
head(bank.df)

set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]


#a
pivot1<-train.df %>%
    group_by(CreditCard,Personal.Loan,) %>%
    summarize(
      cc=sum(Online)
    )

t<-119+469
#b
47/t*100
# 7.993197 %

#c
pivot2<-train.df %>%
    group_by(Personal.Loan) %>%
    summarize(
      online=sum(Online)
    )

pivot3<-train.df %>%
    group_by(Personal.Loan) %>%
    summarize(
      cc=sum(CreditCard)
    )
#d
#i
77/878
#0.08769932
#ii
166/1754
#0.09464082
#iii
635/1754
#0.3620296
#iv
801/878
#0.9123007
#v
1588/1754
# 0.9053592
#vi
1588/1754
#0.9053592

#f
(0.08769932*0.09464082*0.3620296)/((0.08769932*0.09464082*0.3620296)+(0.9123007*0.9053592*0.9053592))
#0.004002187

#g
loan.nb <- naiveBayes(Personal.Loan ~ Online+CreditCard, data = train.df)
loan.nb
#It would appear i made a small error somewhere but definetely streamlines the process.
#0.09166667

#8.2
