## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE------------------------------------------------------------

dir.create("inst/doc")
file.copy(dir("vignettes", full.names=TRUE), "inst/doc", overwrite=TRUE)

library(CoFESSNARX)
library(dplyr)

set.seed(123)
N <- 450 
n <- seq(1:N)
a <- n/10+4*sin(n/10)+sample(-1:6,N,replace=T)+rnorm(N)
b <- n/8+4*cos(n/10)+sample(-3:3,N,replace=T)+rnorm(N) 
c <- n/6+4*sin(n/10)+sample(-5:1,N,replace=T)+rnorm(N)
y <- (a+b+c)/3+rnorm(N)


date<-seq(as.Date("2018-1-1"), as.Date("2019-3-26"), by="days")

x <- data.frame(date,a,b,c)  
y <- data.frame(date, y)

dx <- 2 # The AR order for the Exegenous series 
dy <- 2 # The AR order for the Target series 
delay <- 10 # The number of time steps that the target is in the future
date <- T # Date or no Date Boolean

data<-CoFESSNARX::CoFESNARXdata(y,x,dy,dx,delay,date)

head(data)


y <- CoFESSNARX::returnTARGET(data)
x <- CoFESSNARX::returnX(data,T)
print(head(y))
print(head(x))

## ----warning=FALSE------------------------------------------------------------

library(CoFESSNARX)
library(dplyr)
set.seed(123)
N <- 450 
n <- seq(1:N)
a <- n/10+4*sin(n/10)+sample(-1:6,N,replace=T)+rnorm(N)
b <- n/8+4*cos(n/10)+sample(-3:3,N,replace=T)+rnorm(N) 
c <- n/6+4*sin(n/10)+sample(-5:1,N,replace=T)+rnorm(N)
y <- (a+b+c)/3+rnorm(N)


x <- data.frame(a,b,c)
y <- data.frame(y)


dx <- 2 # The AR order for the Exegenous series 
dy <- 2 # The AR order for the Target series 
delay <- 10 # The number of time steps that the target is in the future
date <- F # Date or no Date Boolean

data<-CoFESSNARX::CoFESNARXdata(y,x,dy,dx,delay,date)

head(data)


y <- CoFESSNARX::returnTARGET(data)
x <- CoFESSNARX::returnX(data,F)
print(head(y))
print(head(x))

## ----warning=FALSE------------------------------------------------------------

library(CoFESSNARX)
library(dplyr)

set.seed(123)
N <- 450 
n <- seq(1:N)
a <- n/10+4*sin(n/10)+sample(-1:6,N,replace=T)+rnorm(N)
b <- n/8+4*cos(n/10)+sample(-3:3,N,replace=T)+rnorm(N) 
c <- n/6+4*sin(n/10)+sample(-5:1,N,replace=T)+rnorm(N)
y <- (a+b+c)/3+rnorm(N)


date<-seq(as.Date("2018-1-1"), as.Date("2019-3-26"), by="days")

x <- data.frame(date,a,b,c)
y <- data.frame(date, y)


dx <- 2 # The AR order for the Exegenous series 
dy <- 2 # The AR order for the Target series 
delay <- 10 # The number of time steps that the target is in the future
date <- T # Date or no Date Boolean
s <- 12

data<-CoFESSNARX::CoFESSNARXdata(y,x,dy,dx,delay,s,date)

head(data)


y <- CoFESSNARX::returnTARGET(data)
x <- CoFESSNARX::returnX(data,T)
print(head(y))
print(head(x))

## ----warning=FALSE------------------------------------------------------------

library(CoFESSNARX)
library(dplyr)

set.seed(123)
N <- 450 
n <- seq(1:N)
a <- n/10+4*sin(n/10)+sample(-1:6,N,replace=T)+rnorm(N)
b <- n/8+4*cos(n/10)+sample(-3:3,N,replace=T)+rnorm(N) 
c <- n/6+4*sin(n/10)+sample(-5:1,N,replace=T)+rnorm(N)
y <- (a+b+c)/3+rnorm(N)


x <- data.frame(a,b,c)
y <- data.frame(y)


dx <- 2 # The AR order for the Exegenous series 
dy <- 2 # The AR order for the Target series 
delay <- 10 # The number of time steps that the target is in the future
date <- F # Date or no Date Boolean
s <- 12
data<-CoFESSNARX::CoFESSNARXdata(y,x,dy,dx,delay,s,date)

head(data)


y <- CoFESSNARX::returnTARGET(data)
x <- CoFESSNARX::returnX(data,F)
#x <- CoFESSNARX::returnX(data,F)
print(head(y))
print(head(x))

