load("bigsmartsales.RData")
library(dummies)
library(tidyverse)
library(broom)
library(glmnet)
library(corrplot)
library(ggplot2)
library(Hmisc)
library(standardize)
library(vegan)
library(scales)
library(VIF)
train<-read.csv("train.csv",header=T)
 ab<-is.na(train)
col_names = colnames(ab)
col2<-data.frame()
for (i in 1:ncol(train)){
  col2[i,1] = col_names[i]
  col2[i,2] = nrow(ab[which(ab[,i] == TRUE),])
}
# na_count <-sapply(train, function(y) sum(length(which(is.na(y)))))
# na_count <- data.frame(na_count)
train$Item_Weight<-impute(train$Item_Weight,mean)
sum(is.na(train$Item_Weight))

train$Item_Weight <- as.numeric(train$Item_Weight)
str(train)
training<-as.data.frame(train)
training$Item_Fat_Content<-gsub(pattern="LF",replacement = "Low Fat",x= training$Item_Fat_Content)
training$Item_Fat_Content<-gsub(pattern="reg",replacement = "Regular",x= training$Item_Fat_Content)
training$Item_Fat_Content<-gsub(pattern="low fat",replacement = "Low Fat",x= training$Item_Fat_Content)
training_num<- training[,c(2,4,6)]
cor(training_num, use="all.obs", method="spearman")
cor(training_num, use="all.obs", method="kendall")
cor(training_num, use="all.obs", method="pearson")
#corrplot(training_num)
#Plotting numerical indepedent variables
# wt<-density(training$Item_Weight,kernel=c("gaussian"))
# plot(wt,lwd=2,main="Plot for Weight of item")
# polygon(wt, col="red", border="blue")
# visible<-density(training$Item_Visibility,kernel=c("gaussian"))
# plot(visible,lwd=2,main="Plot for Visibility of item")
# polygon(visible, col="red", border="blue")
# mrp<-density(training$Item_MRP,kernel=c("gaussian"))
# plot(mrp,lwd=2,main="Plot for Price of item")
#polygon(mrp, col="yellow", border="black")
#standardizing numerical variables
training$Item_Visibility<-sqrt(train$Item_Visibility)
plot(density(train$Item_Visibility),main="After sqrt transformation")
training$Item_MRP<-log(train$Item_MRP)
#plot(density(train$Item_MRP),main="After log transformation")
#Item_Visibility<-exp_trans(base=exp(1))
#T_cub = sign(Turbidity) * abs(Turbidity)^(1/3)
#plotting  categorical variables after dummification

# outlier values.
train1<-data.frame()
train2<-data.frame()
train3 <- training[0,1]

for (i in 1:ncol(training)){
if(class(training[,i])=="numeric"||class(training[,i])=="integer" ){
train2<-training[,i]
print(i)
train3 <- cbind(train3,train2)
}
}

train3<-as.data.frame(train3)
train3 <- train3[,-c(1)]
colnames(train3)<-c("Item_Weight","Item_Visibility","Item_MRP")
outlier_values1<- boxplot.stats(train3$Item_Weight)$out  
outlier_values2<-  boxplot.stats(train$Item_MRP)$out  
outlier_values3<-  boxplot.stats(train3$Item_Visibility)$out  
# boxplot(train3$Item_Visibility,main="Box Plots for Visibility", boxwex=0.1)
# boxplot(train3$Item_MRP, main="Box Plots for MRP", boxwex=0.1)
# boxplot(train3$Item_Weight, main="Box Plots for Weight", boxwex=0.1)


training <- dummy.data.frame(training, names=c("Outlet_Size","Outlet_Location_Type","Outlet_Type"), sep="_")


# df1 <- data.frame(coln= "",value="")
# df2 <- data.frame(coln= "",value="")
# for(i in colnames(train)){
#   df1$coln <- i
#   df1$value <- sum(is.na(train[,i]))
#   print(df1)
#   df2 <- rbind(df1,df2)
#   print(df2)
# }
# df2 <-df2[-c(13),]


#Linear model


linear <- lm(Item_Outlet_Sales~.-Item_Identifier-Item_Fat_Content-Item_Type-Outlet_Size_Medium-Outlet_Identifier-Outlet_Establishment_Year-`Outlet_Location_Type_Tier 3`-Outlet_Size_Small-`Outlet_Type_Supermarket Type3`    , data=training)
vif(linear)
summary(linear)
plot(linear)
y1 <- training$Item_Outlet_Sales
x1 <- training %>% select(Item_Weight,Item_Visibility,Item_MRP,Outlet_Size_ ,Outlet_Size_High,`Outlet_Location_Type_Tier 1`,`Outlet_Location_Type_Tier 2`,`Outlet_Type_Grocery Store`,`Outlet_Type_Supermarket Type1`,`Outlet_Type_Supermarket Type2` ) %>% data.matrix()
lambdas <- 10^seq(3, -2, by = -.1)
ridge1 <- glmnet(x1, y1, alpha = 0, lambda = lambdas)
class(ridge)
summary(ridge)
cv_fit<- cv.glmnet(x1, y1, alpha = 0, lambda = lambdas)
plot(cv_fit)
opt_lambda <- cv_fit$lambda.min
opt_lambda
training <- as.data.frame(training)

pred_training<- predict(ridge1, s = opt_lambda, newx = x1)
sst <- sum(y1^2)
sse <- sum((pred_training - y1)^2)
rsq <- 1 - sse / sst
rsq
#lasso regression
lasso<-glmnet(x1, y1, alpha = 1, lambda = lambdas)
cv_fit<- cv.glmnet(x1, y1, alpha = 1, lambda = lambdas)
opt_lambda <- cv_fit$lambda.min
pred_training_lasso<- predict(lasso, s = opt_lambda, newx = x1)
sst <- sum(y1^2)
sse <- sum((pred_training_lasso - y1)^2)
rsq <- 1 - sse / sst
rsq




#Testing on test data
testdata<-read.csv("Test.csv",header=T)
testdata <- dummy.data.frame(testdata, names=c("Outlet_Size","Outlet_Location_Type","Outlet_Type"), sep="_")
x2<-testdata %>% select(Item_Weight,Item_Visibility,Item_MRP,Outlet_Size_ ,Outlet_Size_High,`Outlet_Location_Type_Tier 1`,`Outlet_Location_Type_Tier 2`,`Outlet_Type_Grocery Store`,`Outlet_Type_Supermarket Type1`,`Outlet_Type_Supermarket Type2` ) %>% data.matrix()
tested_by_linear<-predict(linear,newx= testdata)
write.csv(tested_by_linear,"C:\\Users\\Abeer\\Desktop\\Linear_Output.csv")
tested_by_ridge<-predict(ridge1,s=opt_lambda,newx = x2)
write.csv(tested_by_ridge,"C:\\Users\\Abeer\\Desktop\\Ridge_Output.csv")
tested_by_lasso<-predict(lasso,s=opt_lambda,newx = x2)
write.csv(tested_by_lasso,"C:\\Users\\Abeer\\Desktop\\Lasso_Output.csv")
