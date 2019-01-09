require(moments)
require(stats)
require(ggplot2)
library(MASS)
library(car)
library(forecast)
library(glmnet)


df = read.csv("/Users/poojachopra/Desktop/pie_thon/jupyter notebooks/SeasonalVolume.csv")
dim(df)
head(df)
summary(df)

df <- df[df$Net.Total >= 0, ]


hist(as.numeric(df$Net.Total),
     breaks=12,
     xlab="Monthly Sales",
     main="Histogram of Monthly Sales",
     border ="blue")

hist(as.numeric(df$Quantity),
     breaks=12,
     xlab="Monthly Sales",
     main="Histogram of Monthly Sales",
     border ="blue")

# Aggregate Sales by Business Day
# total_sales_1<- aggregate(Net.Total~date, data=df,sum)
# par(mfrow=c(1,2))
# ggplot(data=total_sales_1,aes(x=date
#                               ,y=Net.Total, 
#                               colour=Net.Total))+geom_line()+
#   ggtitle("SALES ($) by BUSINESS DAY")+theme_bw()

par(mfrow=c(1,1))

boxplot(Net.Total~date,data=total_sales_1,
        main="Sales by Date",
        ylab="Total Sales",
        xlab="Date",
        border=c("red","blue","black"))

boxplot(Quantity~date,data=df,
        main="Sales by Date",
        ylab="Total Sales",
        xlab="Date",
        border=c("red","blue","black"))

ggplot(data = df, aes(x = Quantity, y = Net.Total))+
  geom_point(aes(color = DAY_PART),size = 2) + 
  ggtitle("SALES and Service Time Plot")+
  theme_bw()

ggplot(data = df, aes(x = Quantity, y = Net.Total))+
  geom_point(aes(color = Department),size = 2) + 
  ggtitle("SALES and Service Time Plot")+
  theme_bw()

ggplot(data = df, aes(x = Quantity, y = Net.Total))+
  geom_point(aes(color = SEASONAL),size = 2) + 
  ggtitle("SALES and Service Time Plot")+
  theme_bw()

#cheaper items more sale
par(mfrow=c(1,1))
ggplot(data=df, aes(Net.Total)) + 
  geom_histogram(col="cornflowerblue",
                 fill="white",
                 alpha = .2) + 
  labs(title="Histogram for SALES $") +
  labs(x="SALES", y="Count")+theme_bw()

# The Number of Customer by Day Part

ggplot(data=df,aes(x=DAY_PART
                       ,y=Quantity))+
  ggtitle("Number of Customer by DAY PART")+
  theme_bw()+
  geom_boxplot(aes(colour = DAY_PART))

# The number of customer by DAYOFWEEK

ggplot(data=df,aes(x=WeekD
                       ,y=Quantity))+
  ggtitle("Number of Customer by DAY OF WEEK")+
  theme_bw()+
  geom_boxplot(aes(colour = WeekD))

# the number of customer by MONTH

boxplot(Quantity~MonthN,data=df,
        main="Number of Customer by Month",
        ylab="Number of Customer",
        xlab="MONTH",
        border=c("red","blue","green","yellow","orange","purple","black"))

# number of customers by YEAR

boxplot(Avg.Sale.per.Product~year,data=df,
        main="Average Sales by YEAR",
        ylab="Average Sales Per Product",
        xlab="YEAR",
        border=c("red","blue","green","yellow","orange","purple","black"))

#************************************************************************************
#Analyze the average sales per ticket
#************************************************************************************

library(pivottabler)

qhpvt(df, c("year", "MonthN"), NULL,  
      c("Total Sales"="sum(Net.Total)", "Std Dev"="sd(Net.Total)"),
      formats=list("%.2f", "%.1f"))

qhpvt(df, c("year"), NULL,  
      c("AVERAGE SALES PER ITEM"="mean(Avg.Sale.per.Product)"),
      formats=list("%.2f"))

qhpvt(df, c("MonthN"), NULL,  
      c("AVERAGE SALES PER ITEM"="mean(Avg.Sale.per.Product)"),
      formats=list("%.2f"))

qhpvt(df, c("WeekD"), NULL,  
      c("AVERAGE SALES PER ITEM"="mean(Avg.Sale.per.Product)"),
      formats=list("%.2f"))

#Average sales per item by day part
ggplot(data=df,aes(x=DAY_PART
                       ,y=Avg.Sale.per.Product))+
  ggtitle("Average Sales Per Item by DAY PART")+
  theme_bw()+
  geom_boxplot(aes(colour = DAY_PART))


# Average sales per day part and day of week
average_sales_daypart<- aggregate(Net.Total~WeekD+DAY_PART, data=df,mean)
par(mfrow=c(1,1))
ggplot(data=average_sales_daypart,aes(x=WeekD
                                      ,y=Net.Total
                                      ,group=DAY_PART
                                      ,colour=DAY_PART))+
  geom_line()+
  ggtitle("AVERAGE SALES ($) by DAY OF WEEK and DAYPART")+
  theme_bw()+
  geom_point(size=3)

qqnorm(df$Net.Total, ylab = "Sales",
       main = "Q-Q Plot of Density of Sales", col = "red")
qqline(df$Net.Total,col="black")

#Aggregate Total Customer data by Business Day and Day Part
total_customer<- aggregate(Quantity~date+DAY_PART, data=df,sum)
par(mfrow=c(1,1))
ggplot(data=total_customer,aes(x=date
                               ,y=Quantity
                               ,group=DAY_PART
                               ,colour=DAY_PART))+geom_line()+
  ggtitle("Number of Customer by DAY PART for BUSINESS DAY")+theme_bw()

# Average NUmber of Customer for Day of WEEK
average_customer<- aggregate(Quantity~WeekD+DAY_PART, data=df,mean)
par(mfrow=c(1,1))
ggplot(data=average_customer,aes(x=WeekD
                                 ,y=Quantity
                                 ,group=DAY_PART
                                 ,colour=DAY_PART))+geom_line()+
  ggtitle("Weekly Number of Customer by Day part")+
  theme_bw()+
  geom_point(size=3)

# Average NUmber of Customer for Month

average_customer1<- aggregate(Quantity~MonthN+DAY_PART, data=df,mean)
par(mfrow=c(1,1))
ggplot(data=average_customer1,aes(x=MonthN
                                  ,y=Quantity
                                  ,group=DAY_PART
                                  ,colour=DAY_PART))+geom_line()+
  ggtitle("Monthly average number of Customer by Day part")+
  theme_bw()+
  geom_point(size=3)


library(fastDummies)

#when we want to use neural networks
# Mydata_dummies = fastDummies::dummy_cols(df, select_columns = c("Overall_Sat_Scale_Ctgy",                      
#                                                                        "quarter",                    
#                                                                        "Warranty_Grp", "Mkt_Team_Name",                      
#                                                                        "Disposition_Ind" , "Repair_Type_Cd",                     
#                                                                        "comptia",
#                                                                        "mail_in_program_name" ) )    

# split data into test/train

smp_size <- floor(0.60 * nrow(df))
## set the seed to make your partition reproducible
set.seed(123)

train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train_df <- df[train_ind, ]
test_df_data <- df[-train_ind, ]
nrow(train_df)
nrow(test_df_data)
dim(train_df)
dim(test_df_data)
str(train_df)
str(test_df_data)

# Model Multivariate linear regression
fit_lm<-lm(Net.Total~.,data=train_df)

# plot model and residuals
par(mfrow=c(2,2))
plot(fit_lm)
summary(fit_lm)

res_lm<-residuals(fit_lm)
par(mfrow=c(1,1))
plot(res_lm
     ,col='red'
     ,xlab='Period'
     ,ylab='Residual'
     ,main='Residual of Linear Model')
abline(0,0)

#showed equally distributed along both sides 

predict_lm<- predict(fit_lm, newdata =test_df_data)

#Model 2
#Gradient Boosting Model
#does not let model get stuck in local minima

library(gbm)
set.seed(1)

gbm_model<- gbm(Net.Total ~ ., data = train_df, distribution="gaussian",
                n.trees = 3000, shrinkage=0.005, interaction.depth = 4) # 1205 11945 err: 0.10


summary(gbm_model)

gbm_pred <- predict.gbm(gbm_model, newdata = test_df_data,
                        n.trees = 3000)

plot(test_df_data$Net.Total
     ,type='l'
     ,col=2)
lines(gbm_pred, col=3)

#red actual 
#green predicted

#when we want to use neural networks
Mydata_dummies = fastDummies::dummy_cols(df, select_columns = c("Department",                      
                                                                       "SEASONAL",                    
                                                                       "DAY_PART", "WeekD" ) )             


#For RF
drop_var<-c(
  "Department", "SEASONAL","DAY_PART")

##################################################################################
Mydata_db<-Mydata_dummies[,!(names(Mydata_dummies)%in%drop_var)]  

# split data into test/train

smp_size <- floor(0.60 * nrow(Mydata_db))
## set the seed to make your partition reproducible
set.seed(123)

train_ind_db <- sample(seq_len(nrow(Mydata_db)), size = smp_size)

train_db <- Mydata_db[train_ind_db, ]
test_db_data <- Mydata_db[-train_ind_db, ]
nrow(train_db)
nrow(test_db_data)
dim(train_db)
dim(test_db_data)
str(train_db)
str(test_db_data)

# Model 2 - Random Forest***********************************
library (randomForest)
fit_rf<- randomForest(Net.Total~.
                              ,data=train_db
                              ,mtry = 5
                              ,importance =TRUE
                              , type = "regression")
# delete cold drinks, gift card, hot food 
drop_var<-c(
  "Department_Cold Drinks")

train_db<-train_db[,!(names(train_db)%in%drop_var)]  

drop_var<-c(
  "Department_Dry Pastries")
train_db<-train_db[,!(names(train_db)%in%drop_var)]  

drop_var<-c(
  "Department_Gift Card")
train_db<-train_db[,!(names(train_db)%in%drop_var)]  

drop_var<-c(
  "Department_Hot Food")
train_db<-train_db[,!(names(train_db)%in%drop_var)]  

drop_var<-c(
  "Department_Type in")
train_db<-train_db[,!(names(train_db)%in%drop_var)]  


fit_rf

importance(fit_rf)

varImpPlot(fit_rf_abilene)

set.seed(1)

