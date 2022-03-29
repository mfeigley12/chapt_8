setwd("~/2022 Spring/PREDICTIVE ANALYTICS/dmba-datasets/dmba")
library(e1071)
library(dplyr)
library(tidyverse)
library(tidyr)
library(caret)


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
accident.df <- read.csv("Accidents.csv")
view(accident.df)
head(accident.df)
#accident.df<-dummy_columns(accident.df,select_columns = "MAX_SEV",remove_selected_columns = TRUE,
#split = c("no-injury","non-fatal"))

#injury<-accident.df[,MAX_SEV:=ifelse(MAX_SEV=="non-fatal",1,
 #                                    ifelse(non-fatal=="fatal",1
  #                                   else(0)))
   #                 ]

accident.df$injury<-ifelse(accident.df$MAX_SEV == "no-injury",0,1)
accident.df$injury<-as.factor(accident.df$injury)
accident.df$MAX_SEV<-as.factor(accident.df$MAX_SEV)

set.seed(1)
train.index <- sample(c(1:dim(accident.df)[1]), dim(accident.df)[1]*0.6)
train.df <- accident.df[train.index, ]
valid.df <- accident.df[-train.index, ]

#a
sum(accident.df$injury)
308/600
#0.5133333 or 51%
#should be 51% since its 308 out of 600 total cases that involved an injury.

#bi
a<-train.df[1:12,]
a

pivot4<-train.df %>%
    group_by(injury) %>%
    summarize(
      cc=count(WEATHER_adverse)
    )
#ii
injury.nb <- naiveBayes(injury ~., data = a)
injury.nb
#0.25

#iii

#iv

#v
#They are more accurate using naiveBayes

#c
total.nb <- naiveBayes(injury ~., data = train.df)
total.nb

pred.class <- predict(total.nb, newdata = train.df)
confusionMatrix(pred.class, train.df$injury)

pred.class <- predict(total.nb, newdata = valid.df)
confusionMatrix(pred.class, valid.df$injury)

#99.58% accurate
#0.0014 improvement
#no idea what last question is.
