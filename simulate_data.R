# Simulate Data
library(tidyverse)
library(MASS) # mvrnorm
library(kernlab)
rm(list=ls())
set.seed(501)


# number of individuals
n=1000

# Create binary Demographic Profiles
x1<- rbinom(n=n,size = 1, prob=.5)
x2<- rbinom(n=n,size = 1, prob=.75)
x3<- rbinom(n=n,size = 1, prob=.15)

# If you want a trichotomous/categorical variable
# x3<- sample(x=c(1:3),size=n, replace=T) 

df_individual<-data.frame(x1,x2,x3)




# Compute the Squared Exponential Kernel
rbf <- rbfdot(sigma=1)
K<-kernelMatrix(kernel=rbf, as.matrix(df_individual))



#  Take one draw from multivariate normal distribution
y_star1<-mvrnorm(n=1, mu=rep(0,n), Sigma = K)
y_star2<-mvrnorm(n=1, mu=rep(0,n), Sigma = K)
y_star3<-mvrnorm(n=1, mu=rep(0,n), Sigma = K)
y_star4<-mvrnorm(n=1, mu=rep(0,n), Sigma = K)

j1<-rbinom(n,size=1,prob=plogis(y_star1))
df_individual$j1<-j1
j2<-rbinom(n,size=1,prob=plogis(y_star2))
df_individual$j2<-j2
j3<-rbinom(n,size=1,prob=plogis(y_star3))
df_individual$j3<-j3
j4<-rbinom(n,size=1,prob=plogis(y_star4))
df_individual$j4<-j4



# Individual Level Dataset
df_individual


# Group-Level Dataset 
df_individual %>%
  group_by(x1,x2,x3) %>%
  summarise(j1 = sum(j1),j2 = sum(j2),j3 = sum(j3),j4 = sum(j4),
            total=n())->df_group

df_group

write_csv(df_group, file = "df_group.csv")



