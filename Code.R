rm(list=ls())
setwd('/Users/Yang Song/Desktop/data')

#######################
#library
library(dplyr)
library(ggplot2)
library(reshape)
library(xgboost)
library(grid)
library(readr)
library(glmnet)

set.seed(1000)

#######################
#self define function
# muliple figures for ggplot2
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



#read data
Traffic=read.csv(file='Traffic.csv',header=T,stringsAsFactors = F)
Market=read.csv(file='Market.csv',header = T,stringsAsFactors = F)
Sales_Report=read.csv(file='Sales_Report.csv',header=T,stringsAsFactors = F)
SKUs=read.csv(file='SKUs.csv',header=T,stringsAsFactors = F)
Media_Spend=read.csv(file='Media_Spend.csv',header=T,stringsAsFactors = F)

#rename the columns for convenience
colnames(Traffic)=c('Date','Visitors','Visits','Page_Views')
colnames(Market)=c('Date','Visit_T_Com_S','Social_Media_M')
colnames(Sales_Report)=c('Date','Transaction_ID','SKU')
colnames(SKUs)=c('SKU','Product','Value')
colnames(Media_Spend)=c('Date','GRP_OOH','PPC_Spend','Display_Spend','Email_Spend')

###########################################
#merge data part
###########################################
#remove the last obs of Market data
Market=Market[1:(nrow(Market)-1),]

#sum up visitors, visits, page_view by date
str(Traffic)
Traffic_D=Traffic%>%
          group_by(Date)%>%
          summarise(Visitors=sum(Visitors),
                    Visits=sum(Visits),
                    Page_Views=sum(Page_Views))

#merge Market & Traffic data together
com_Market_Traffic=merge(Market,Traffic_D,by='Date')

#merge Sales_Report & SKUs
com_Sales_Report=merge(Sales_Report,SKUs,by='SKU')
#sum up sales by date
com_Sales_Report_D=com_Sales_Report%>%group_by(Date)%>%summarise(Sales=sum(Value))

#Media_Spend turn var into numeric var
str(Media_Spend)
Media_Spend$GRP_OOH=as.numeric(Media_Spend$GRP_OOH)
Media_Spend$PPC_Spend=as.numeric(Media_Spend$PPC_Spend)
Media_Spend$Email_Spend=as.numeric(Media_Spend$Email_Spend)

#merge data together
com_data=merge(com_Market_Traffic,com_Sales_Report_D,by='Date')
com_data=merge(com_data,Media_Spend,by='Date')


#Turn date into date.format in r
com_data$Date_D=as.Date(com_data$Date,format='%d-%b-%y')
com_data=com_data%>%arrange(Date_D)
#reorder the columns
com_data=com_data[c(
  "Date","Date_D","Visit_T_Com_S","Social_Media_M","Visitors",   
  "Visits","Page_Views","PPC_Spend","Display_Spend","Email_Spend","GRP_OOH","Sales"    
)]


#######################################
#Check missing value
#######################################
sum(is.na(com_data))     #total 46 missing value

for(x in names(com_data)){print(paste(x,'missing value is',sum(is.na(com_data[x]))))}
# [1] "Date missing value is 0"
# [1] "Date_D missing value is 0"
# [1] "Visit_T_Com_S missing value is 0"
# [1] "Social_Media_M missing value is 0"
# [1] "Visitors missing value is 0"
# [1] "Visits missing value is 0"
# [1] "Page_Views missing value is 0"
# [1] "PPC_Spend missing value is 0"
# [1] "Display_Spend missing value is 0"
# [1] "Email_Spend missing value is 11"
# [1] "GRP_OOH missing value is 35"
# [1] "Sales missing value is 0"

# Constuct xgboost model to impute mising value
# Email Spend 
Email_Spend_Hist_0=ggplot(com_data,aes(Email_Spend))+geom_histogram(aes(fill = ..count..),binwidth = 10)+ xlim(0,1300)+
  labs(x="Email Spend",title="histogram of Email Spend before imputation")

variable_names=c("Visit_T_Com_S","Social_Media_M","Visitors",
                 "Visits","Page_Views","PPC_Spend","Display_Spend",
                 "Sales")
train_1=com_data[which(is.na(com_data$Email_Spend)==F),]
Email_Spend_xgb=xgboost(data = data.matrix(train_1[,variable_names]),
                label= train_1$Email_Spend,
                nrounds=500,
                objective="reg:linear",
                eval_metric="rmse")
com_data[is.na(com_data$Email_Spend),'Email_Spend']=round(predict(Email_Spend_xgb,
                                                        data.matrix(com_data[is.na(com_data$Email_Spend),variable_names])))
Email_Spend_Hist_1=ggplot(com_data,aes(Email_Spend))+geom_histogram(aes(fill = ..count..),binwidth = 10)+ xlim(0,1300)+
  labs(x="Email Spend",title="histogram of Email Spend after imputation")
multiplot(Email_Spend_Hist_0, Email_Spend_Hist_1, cols=1)

# GRP_OOH
GRP_OOH_Hist_0=ggplot(com_data,aes(GRP_OOH))+geom_histogram(aes(fill = ..count..),binwidth = 1000)+
  xlim(3999,201000)+labs(x="GRP OOH",title="histogram of GRP OOH before imputation")

variable_names=c("Visit_T_Com_S","Social_Media_M","Visitors",
                 "Visits","Page_Views","PPC_Spend","Display_Spend",
                 "Sales")
train_2=com_data[which(is.na(com_data$GRP_OOH)==F),]

GRP_OOH_xgb=xgboost(data = data.matrix(train_2[,variable_names]),
                        label= train_2$GRP_OOH,
                        nrounds=500,
                        objective="reg:linear",
                        eval_metric="rmse")
com_data[is.na(com_data$GRP_OOH),'GRP_OOH']=round(predict(GRP_OOH_xgb,
                                                    data.matrix(com_data[is.na(com_data$GRP_OOH),variable_names])))
GRP_OOH_Hist_1=ggplot(com_data,aes(GRP_OOH))+geom_histogram(aes(fill = ..count..),binwidth = 1000)+ 
  xlim(3999,201000)+labs(x="GRP OOH",title="histogram of GRP OOH after imputation")
multiplot(GRP_OOH_Hist_0, GRP_OOH_Hist_1, cols=1)

# double check
sum(is.na(com_data))    #no missing value

##################################################
# After impute missing value, we have clean dataset
##################################################
# save(com_data,file='clean_com_data.Rdata')
# rm(list=ls())
# load('clean_com_data.Rdata')




# muliple figures for ggplot2
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



##########################
# Descriptive Analysis
##########################

# create new features
com_data$Year=as.numeric(format(com_data$Date_D,'%Y'))
com_data$Month=as.character(format(com_data$Date_D,'%b'))
com_data$Month_n=as.numeric(format(com_data$Date_D,'%m'))
com_data$Quarter=ifelse(com_data$Month_n %in% c(1,2,3), 'Q1',
                        ifelse(com_data$Month_n %in% c(4,5,6), 'Q2',
                               ifelse(com_data$Month_n %in% c(7,8,9), 'Q3','Q4')))
com_data$Year_Month=paste(com_data$Year,com_data$Month_n,sep='_')
com_data$total_Spend=com_data$PPC_Spend+com_data$Display_Spend+com_data$Email_Spend
names(com_data)

#correlation
# Melt the correlation matrix
cor_data=com_data[,c('Visitors','Visits','Page_Views','PPC_Spend','Display_Spend',
                     'Email_Spend','GRP_OOH','Sales')]
cormat = round(cor(cor_data),3)
melted_cormat = melt(cormat, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat , aes(X1, X2, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+ labs(title="correlation heat matrix")




#seasonal pattern
Sales_Year_Month=com_data%>%group_by(Year,Month_n)%>%
                 summarise(Sales=sum(Sales),Spend=sum(total_Spend))%>%
                 arrange(Year,Month_n)
# write.csv(Sales_Year_Month,file='Sales_Year_Month.csv')
plot(ts(Sales_Year_Month$Sales, frequency=12, start=c(2008,1)))
lines(ts(Sales_Year_Month$Spend, frequency=12, start=c(2008,1)),col='red')

#check relation between spending and sales
#total sales
sales_month=com_data%>%
            group_by(Month)%>%
            summarise(sum_sales=sum(Sales),sum_total_spend=sum(total_Spend))
ggplot(sales_month, aes(x= sum_total_spend, y= sum_sales, colour="green", label=Month))+
  geom_point() +geom_text(aes(label=Month),hjust=0, vjust=0)+labs(x='Total Spending',y='Total Sales',title='Spending VS Sales Monthly')
#Email Spending 
sales_month_E=com_data%>%
  group_by(Month)%>%
  summarise(sum_sales=sum(Sales),sum_Email_Spend=sum(Email_Spend))
ggplot(sales_month_E, aes(x= sum_Email_Spend, y= sum_sales, colour="green", label=Month))+
  geom_point() +geom_text(aes(label=Month),hjust=0, vjust=0)+
  labs(x='Total Email Spending',y='Total Sales',title='Email Spending VS Sales Monthly')
#Display Spending
sales_month_D=com_data%>%
  group_by(Month)%>%
  summarise(sum_sales=sum(Sales),sum_Display_Spend=sum(Display_Spend))
ggplot(sales_month_D, aes(x= sum_Display_Spend, y= sum_sales, colour="green", label=Month))+
  geom_point() +geom_text(aes(label=Month),hjust=0, vjust=0)+
  labs(x='Total Display Spending',y='Total Sales',title='Display Spending VS Sales Monthly')
#PPC_Spend
sales_month_P=com_data%>%
  group_by(Month)%>%
  summarise(sum_sales=sum(Sales),sum_PPC_Spend=sum(PPC_Spend))
ggplot(sales_month_P, aes(x= sum_PPC_Spend, y= sum_sales, colour="green", label=Month))+
  geom_point() +geom_text(aes(label=Month),hjust=0, vjust=0)+
  labs(x='Total PPC Spending',y='Total Sales',title='PPC Spending VS Sales Monthly')


########################################
#How to increase sales
########################################
# Total Spend
ggplot(com_data, aes(x= total_Spend, y= Sales))+
  geom_point()+labs(x='Total Spend',y='Sales',title='Total Spend VS Sales')
Q1=ggplot(com_data[com_data$Month_n %in% c(1,2,3),], aes(x= total_Spend, y= Sales))+
  geom_point()+labs(x='Total Spend',y='Sales',title='Total Spend VS Sales Q1')
Q2=ggplot(com_data[com_data$Month_n %in% c(4,5,6),], aes(x= total_Spend, y= Sales))+
  geom_point()+labs(x='Total Spend',y='Sales',title='Total Spend VS Sales Q2')
Q3=ggplot(com_data[com_data$Month_n %in% c(7,8,9),], aes(x= total_Spend, y= Sales))+
  geom_point()+labs(x='Total Spend',y='Sales',title='Total Spend VS Sales Q3')
Q4=ggplot(com_data[com_data$Month_n %in% c(10,11,12),], aes(x= total_Spend, y= Sales))+
  geom_point()+labs(x='Total Spend',y='Sales',title='Total Spend VS Sales Q4')
multiplot(Q1,Q2,Q3,Q4,cols=2)

Q1_Q2_Q4=ggplot(com_data[com_data$Month_n %in% c(1,2,3,4,5,6,10,11,12),], aes(x= total_Spend, y= Sales))+
         geom_point()+labs(x='Total Spend',y='Sales',title='Total Spend VS Sales Except Q3')
multiplot(Q1_Q2_Q4,Q3,cols=1)

ggplot(com_data[com_data$Month_n %in% c(7,8,9),], aes(x= total_Spend, y= Sales,colour=factor(Month)))+
  geom_point()+labs(x='Total Spend',y='Sales',title='Total Spend VS Sales Q3')

#Email_Spemd/PPC_Spend/Display_Spend
Q1_Q2_Q3_Email=ggplot(com_data[com_data$Month_n %in% c(1,2,3,4,5,6,10,11,12),], aes(x= Email_Spend, y= Sales,colour=factor(Quarter)))+
  geom_point()+labs(x='Email Spend',y='Sales',title='Email Spend VS Sales Except Q3')
Q3_Email=ggplot(com_data[com_data$Month_n %in% c(7,8,9),], aes(x= Email_Spend, y= Sales,colour=factor(Month)))+
  geom_point()+labs(x='Email Spend',y='Sales',title='Email Spend VS Sales Q3')
Q1_Q2_Q3_PPC=ggplot(com_data[com_data$Month_n %in% c(1,2,3,4,5,6,10,11,12),], aes(x= PPC_Spend, y= Sales,colour=factor(Quarter)))+
  geom_point()+labs(x='PPC Spend',y='Sales',title='PPC Spend VS Sales Except Q3')
Q3_PPC=ggplot(com_data[com_data$Month_n %in% c(7,8,9),], aes(x= PPC_Spend, y= Sales,colour=factor(Month)))+
  geom_point()+labs(x='PPC Spend',y='Sales',title='PPC Spend VS Sales Q3')
Q1_Q2_Q3_Display=ggplot(com_data[com_data$Month_n %in% c(1,2,3,4,5,6,10,11,12),], aes(x= Display_Spend, y= Sales,colour=factor(Quarter)))+
  geom_point()+labs(x='Display Spend',y='Sales',title='Display Spend VS Sales Except Q3')
Q3_Display=ggplot(com_data[com_data$Month_n %in% c(7,8,9),], aes(x= Display_Spend, y= Sales,colour=factor(Month)))+
  geom_point()+labs(x='Display Spend',y='Sales',title='Display Spend VS Sales Q3')
multiplot(Q1_Q2_Q3_Email,Q3_Email,Q1_Q2_Q3_PPC,Q3_PPC,Q1_Q2_Q3_Display,Q3_Display,cols=3)

#Linear Model
Email_Sales_EQ3_LM=lm(Sales~Email_Spend,com_data[com_data$Quarter %in% c('Q1','Q2','Q4'),])
summary(Email_Sales_EQ3_LM)

PPC_Sales_EQ3_LM=lm(Sales~PPC_Spend,com_data[com_data$Quarter %in% c('Q1','Q2','Q4'),])
summary(PPC_Sales_EQ3_LM)

Display_Sales_EQ3_LM=lm(Sales~Display_Spend,com_data[com_data$Quarter %in% c('Q1','Q2','Q4'),])
summary(Display_Sales_EQ3_LM)


##################################################
# make prediction on sales and spending in 2010
##################################################

#############################################
#Method 1: Confidence Interval projection methodology
#Sales Projection
Sales_CI=com_data%>%
         group_by(Month_n)%>%
         summarise(Mean_Sales=mean(Sales),
                   std_Sales=sd(Sales),
                   count=n(),
                   std_error=qnorm(0.975)*std_Sales/sqrt(count))
Sales_M_2008=com_data%>%filter(Year==2008)%>%group_by(Month_n)%>%summarise(Mean_Sales_2008=mean(Sales))
Sales_M_2009=com_data%>%filter(Year==2009)%>%group_by(Month_n)%>%summarise(Mean_Sales_2009=mean(Sales))
Sales_M_C=merge(Sales_M_2008,Sales_M_2009,by='Month_n')
Sales_M_C$R_Change=(Sales_M_C$Mean_Sales_2009-Sales_M_C$Mean_Sales_2008)/Sales_M_C$Mean_Sales_2008
Sales_CI=merge(Sales_CI,Sales_M_C,by='Month_n')
Sales_CI=Sales_CI%>%mutate(lower_bound=Mean_Sales*(1+R_Change)-std_error,
                           upper_bound=Mean_Sales*(1+R_Change)+std_error)
# write.csv(Sales_CI,file='Monthly_Sales_Confidence_Interval.csv')

#Email_Spend
Email_Spend_CI=com_data%>%
  group_by(Month_n)%>%
  summarise(Mean_Email_Spend=mean(Email_Spend),
            std_Email_Spend=sd(Email_Spend),
            count=n(),
            std_error=qnorm(0.975)*std_Email_Spend/sqrt(count))
Email_Spend_M_2008=com_data%>%filter(Year==2008)%>%group_by(Month_n)%>%summarise(Mean_Email_Spend_2008=mean(Email_Spend))
Email_Spend_M_2009=com_data%>%filter(Year==2009)%>%group_by(Month_n)%>%summarise(Mean_Email_Spend_2009=mean(Email_Spend))
Email_Spend_M_C=merge(Email_Spend_M_2008,Email_Spend_M_2009,by='Month_n')
Email_Spend_M_C$R_Change=(Email_Spend_M_C$Mean_Email_Spend_2009-Email_Spend_M_C$Mean_Email_Spend_2008)/Email_Spend_M_C$Mean_Email_Spend_2008
Email_Spend_CI=merge(Email_Spend_CI,Email_Spend_M_C,by='Month_n')
Email_Spend_CI=Email_Spend_CI%>%
  mutate(lower_bound=Mean_Email_Spend*(1+R_Change)-std_error,
         upper_bound=Mean_Email_Spend*(1+R_Change)+std_error)
# write.csv(Email_Spend_CI,file='Monthly_Email_Spend_Confidence_Interval.csv')

#PPC_Spend
PPC_Spend_CI=com_data%>%
  group_by(Month_n)%>%
  summarise(Mean_PPC_Spend=mean(PPC_Spend),
            std_PPC_Spend=sd(PPC_Spend),
            count=n(),
            std_error=qnorm(0.975)*std_PPC_Spend/sqrt(count))
PPC_Spend_M_2008=com_data%>%filter(Year==2008)%>%group_by(Month_n)%>%summarise(Mean_PPC_Spend_2008=mean(PPC_Spend))
PPC_Spend_M_2009=com_data%>%filter(Year==2009)%>%group_by(Month_n)%>%summarise(Mean_PPC_Spend_2009=mean(PPC_Spend))
PPC_Spend_M_C=merge(PPC_Spend_M_2008,PPC_Spend_M_2009,by='Month_n')
PPC_Spend_M_C$R_Change=(PPC_Spend_M_C$Mean_PPC_Spend_2009-PPC_Spend_M_C$Mean_PPC_Spend_2008)/PPC_Spend_M_C$Mean_PPC_Spend_2008
PPC_Spend_CI=merge(PPC_Spend_CI,PPC_Spend_M_C,by='Month_n')
PPC_Spend_CI=PPC_Spend_CI%>%
  mutate(lower_bound=Mean_PPC_Spend*(1+R_Change)-std_error,
         upper_bound=Mean_PPC_Spend*(1+R_Change)+std_error)
# write.csv(PPC_Spend_CI,file='Monthly_PPC_Spend_Confidence_Interval.csv')

#Display_Spend
Display_Spend_CI=com_data%>%
  group_by(Month_n)%>%
  summarise(Mean_Display_Spend=mean(Display_Spend),
            std_Display_Spend=sd(Display_Spend),
            count=n(),
            std_error=qnorm(0.975)*std_Display_Spend/sqrt(count))
Display_Spend_M_2008=com_data%>%filter(Year==2008)%>%group_by(Month_n)%>%summarise(Mean_Display_Spend_2008=mean(Display_Spend))
Display_Spend_M_2009=com_data%>%filter(Year==2009)%>%group_by(Month_n)%>%summarise(Mean_Display_Spend_2009=mean(Display_Spend))
Display_Spend_M_C=merge(Display_Spend_M_2008,Display_Spend_M_2009,by='Month_n')
Display_Spend_M_C$R_Change=(Display_Spend_M_C$Mean_Display_Spend_2009-Display_Spend_M_C$Mean_Display_Spend_2008)/Display_Spend_M_C$Mean_Display_Spend_2008
Display_Spend_CI=merge(Display_Spend_CI,Display_Spend_M_C,by='Month_n')
Display_Spend_CI=Display_Spend_CI%>%
  mutate(lower_bound=Mean_Display_Spend*(1+R_Change)-std_error,
         upper_bound=Mean_Display_Spend*(1+R_Change)+std_error)
# write.csv(Display_Spend_CI,file='Monthly_Display_Spend_Confidence_Interval.csv')


#########################################
#Method 2: Modeling method
Q1_Sales=ggplot(com_data[com_data$Quarter=='Q1',],aes(x=Sales))+
     geom_histogram(binwidth=800,aes(y = ..density..,fill=..count..)) +
     scale_fill_gradient('Count',low='#DCDCDC',high="#7C7C7C")+
     stat_function(fun = dnorm, colour = "red",
                    args = list(mean = mean(com_data[com_data$Quarter=='Q1','Sales']),
                    sd = sd(com_data[com_data$Quarter=='Q1','Sales'])))+xlim(5000,25000)+
     labs(title='Histogram of Sales in Q1')
Q2_Sales=ggplot(com_data[com_data$Quarter=='Q2',],aes(x=Sales))+
     geom_histogram(binwidth=500,aes(y = ..density..,fill=..count..)) +
     scale_fill_gradient('Count',low='#DCDCDC',high="#7C7C7C")+
     stat_function(fun = dnorm, colour = "red",
                    args = list(mean = mean(com_data[com_data$Quarter=='Q2','Sales']),
                    sd = sd(com_data[com_data$Quarter=='Q2','Sales'])))+
  xlim(0,12000)+labs(title='Histogram of Sales in Q2')
Q3_Sales=ggplot(com_data[com_data$Quarter=='Q3',],aes(x=Sales))+
     geom_histogram(binwidth=50,aes(y = ..density..,fill=..count..)) +
     scale_fill_gradient('Count',low='#DCDCDC',high="#7C7C7C")+
     stat_function(fun = dnorm, colour = "red",
                    args = list(mean = mean(com_data[com_data$Quarter=='Q3','Sales']),
                    sd = sd(com_data[com_data$Quarter=='Q3','Sales'])))+
     xlim(250,1300)+labs(title='Histogram of Sales in Q3')
Q4_Sales=ggplot(com_data[com_data$Quarter=='Q4',],aes(x=Sales))+
     geom_histogram(binwidth=800,aes(y = ..density..,fill=..count..)) +
     scale_fill_gradient('Count',low='#DCDCDC',high="#7C7C7C")+
     stat_function(fun = dnorm, colour = "red",
                    args = list(mean = mean(com_data[com_data$Quarter=='Q4','Sales']),
                    sd = sd(com_data[com_data$Quarter=='Q4','Sales'])))+
  xlim(1300,20000)+labs(title='Histogram of Sales in Q4')
multiplot(Q1_Sales,Q2_Sales,Q3_Sales,Q4_Sales,cols=2)

#Linear Model for Q3
LM_Q3=lm(Sales~PPC_Spend,data=com_data[com_data$Quarter %in% c('Q3'),])
summary(LM_Q3)

#Linear Model for Q1 Q2 Q4
LM_EQ3=lm(Sales~Email_Spend+PPC_Spend,data=com_data[com_data$Quarter %in% c('Q1','Q2','Q4'),])
summary(LM_EQ3)
#coefficient doesnot make sense, the reason is linear colinearilty

#Lasso regression to deal with colinearity
lasso_model_data=com_data[com_data$Quarter %in% c('Q1','Q2','Q4'),]
mylasso2=cv.glmnet(as.matrix(lasso_model_data[,c('Email_Spend','PPC_Spend')]), 
                   lasso_model_data$Sales, alpha=1,lambda=exp(seq(-6,1,length=100)))
lamda.min=mylasso2$lambda.min
lamda.1sd=mylasso2$lambda.1se
plot(mylasso2)
mylasso = glmnet(as.matrix(lasso_model_data[,c('Email_Spend','PPC_Spend')]), 
                 lasso_model_data$Sales, alpha=1,lambda=lamda.min) 
names(mylasso)
mylasso$beta

#optimization
optim_data=com_data[com_data$Year==2009,
                    c('Date','Date_D','Year','Month','Month_n','Quarter','Email_Spend',
                      'Display_Spend','PPC_Spend','total_Spend','Sales')]
#2009 information
total_Spend_2009_whole_year=sum(optim_data$total_Spend)
Email_Spend_2009_whole_year=sum(optim_data$Email_Spend)
PPC_Spend_2009_whole_year=sum(optim_data$PPC_Spend)
Display_Spend_2009_whole_year=sum(optim_data$Display_Spend)
total_Sales_2009_whole_year=sum(optim_data$Sales)

#2010 optimization
#Q3
optim_data_Q3=optim_data[optim_data$Quarter=='Q3',]
#Q3 Email
optim_data_Q3$Email_Spend_2010=pmin(optim_data_Q3$Email_Spend,100)
optim_data_Q3$Email_Spend_Diff=optim_data_Q3$Email_Spend-optim_data_Q3$Email_Spend_2010
optim_data_Q3$Email_Contri=optim_data_Q3$Email_Spend_Diff*6.660256
#Q3 Display
optim_data_Q3$Display_Spend_2010=pmin(optim_data_Q3$Display_Spend,quantile(optim_data_Q3$Display_Spend,0.25))
optim_data_Q3$Display_Spend_Diff=optim_data_Q3$Display_Spend-optim_data_Q3$Display_Spend_2010
optim_data_Q3$Display_Contri=optim_data_Q3$Display_Spend_Diff*6.660256
#Q3 PPC
optim_data_Q3$PPC_Spend_2010=optim_data_Q3$PPC_Spend+
                             optim_data_Q3$Email_Spend_Diff+
                             optim_data_Q3$Display_Spend_Diff
#2010Q3 combined information
total_Spend_2010_Q3=sum(optim_data_Q3$PPC_Spend_2010)+
                    sum(optim_data_Q3$Display_Spend_2010)+
                    sum(optim_data_Q3$Email_Spend_2010)
Email_Spend_2010_Q3=sum(optim_data_Q3$Email_Spend_2010)
PPC_Spend_2010_Q3=sum(optim_data_Q3$PPC_Spend_2010)
Display_Spend_2010_Q3=sum(optim_data_Q3$Display_Spend_2010)
total_Sales_2010_Q3=sum(optim_data_Q3$Sales)+
                    sum(optim_data_Q3$Email_Contri)+
                    sum(optim_data_Q3$Display_Contri)

#Q1 Q2 Q4
optim_data_EQ3=optim_data[optim_data$Quarter %in% c('Q1','Q2','Q4'),]
#Email
optim_data_EQ3$Email_Spend_2010=optim_data_EQ3$Email_Spend
optim_data_EQ3$Email_Spend_Diff=0
optim_data_EQ3$Email_Contri=0
#Display
optim_data_EQ3$Display_Spend_2010=pmin(optim_data_EQ3$Display_Spend,quantile(optim_data_EQ3$Display_Spend,0.25))
optim_data_EQ3$Display_Spend_Diff=optim_data_EQ3$Display_Spend-optim_data_EQ3$Display_Spend_2010
optim_data_EQ3$Display_Contri=optim_data_EQ3$Display_Spend_Diff*6.6661316348
#PPC
optim_data_EQ3$PPC_Spend_2010=optim_data_EQ3$PPC_Spend+
                              optim_data_EQ3$Display_Spend_Diff
#2010Q3 combined information
total_Spend_2010_Q3=sum(optim_data_EQ3$PPC_Spend_2010)+
  sum(optim_data_EQ3$Display_Spend_2010)+
  sum(optim_data_EQ3$Email_Spend_2010)
Email_Spend_2010_Q3=sum(optim_data_EQ3$Email_Spend_2010)
PPC_Spend_2010_Q3=sum(optim_data_EQ3$PPC_Spend_2010)
Display_Spend_2010_Q3=sum(optim_data_EQ3$Display_Spend_2010)
total_Sales_2010_Q3=sum(optim_data_EQ3$Sales)+
                    sum(optim_data_EQ3$Display_Contri)


#2008-2010 Visualization: Table
data_2010=rbind(optim_data_EQ3,optim_data_Q3)
Sales_Year_Month=data_2010%>%group_by(Year,Month_n)%>%
                 summarise(Sales=sum(Sales)+sum(Email_Contri)+sum(Display_Contri),
                           Spend=sum(Email_Spend_2010)+sum(PPC_Spend_2010)+sum(Display_Spend_2010))%>%
                 arrange(Year,Month_n)
# write.csv(Sales_Year_Month,file='Sales_Year_Month_2010.csv')
