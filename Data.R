#Loading all the necessary libraries
library(readxl)
library(tidyr)
library("MFPCA")
library("ggfortify")
library("ggplot2")
library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)
library(multcomp)
library(car)
source("Readexcel.R")

# Loading the excel files 
mysheets <- read_excel_allsheets("gdi-gps-calculator-v-3-2.xlsx")

# Reading excel sheets from the excel file
# Loading cases and controls dataset

# Cases Dataset
cases<-mysheets$`Subject Kinematics`[,1:8]
id<-c(rep("L",459),rep("R",459))
cases<-data.frame(id,cases)
colnames(cases) <- c("Side", "CurveName","Time",c(1:6))

# Control dataset
controls<-mysheets$`Control Kinematics`[,1:40]
controls<-data.frame(id,controls)
colnames(controls) <- c("Side", "CurveName","Time",c(7:44))
controls<-controls[,-c(1,2,3)]

# Combining cases and control to one dataset
data<-cbind(cases,controls)

# Differentiating cases and controls by adding a column label to which group they belongs
ID = c(rep("Case",6),rep("Control",38))


##~~~~~~~~~~~~~~~~ Segregating left and right side

#~~~~~~~~~~~~ Left leg
leftdata<-data[data$Side=="L", ]

#~~~~ Pelvis angle for three directions x,y and z

# 1st direction==Pelvis Ant/Pst
leftdataNewPelAnglesD1 <-t(leftdata[leftdata$CurveName=="Pelvis Ant/Pst",4:47]) # dim (47,51)
leftdataNewPelAnglesD1 <- data.frame(apply(leftdataNewPelAnglesD1 , 2, function(x) as.numeric(x)))
leftdataNewPelAnglesD1<-cbind(ID,leftdataNewPelAnglesD1)
rownames(leftdataNewPelAnglesD1)=1:44

# 2nd direction==Pelvis Up/Dn
leftdataNewPelAnglesD2 <-t(leftdata[leftdata$CurveName=="Pelvic Up/Dn",4:47]) # dim (47,51)
colnames(leftdataNewPelAnglesD2)=1:51
leftdataNewPelAnglesD2<- data.frame(apply(leftdataNewPelAnglesD2, 2, function(x) as.numeric(x)))
leftdataNewPelAnglesD2<-cbind(ID,leftdataNewPelAnglesD2)
rownames(leftdataNewPelAnglesD2)=1:44

# 3rd direction==Pelvis Int/Ext
leftdataNewPelAnglesD3 <-t(leftdata[leftdata$CurveName=="Pelvic Int/Ext",4:47]) # dim (47,51)
colnames(leftdataNewPelAnglesD3)=1:51
leftdataNewPelAnglesD3 <- data.frame(apply(leftdataNewPelAnglesD3 , 2, function(x) as.numeric(x)))
leftdataNewPelAnglesD3<-cbind(ID,leftdataNewPelAnglesD3)
rownames(leftdataNewPelAnglesD3)=1:44

#~~~~ Hip angle for three directions x,y and z

# 1st direction==Hip Flx/Ext
leftdataHipAnglesD1 <-t(leftdata[leftdata$CurveName=="Hip Flx/Ext",4:47]) # dim (44,51)
colnames(leftdataHipAnglesD1 )=1:51
leftdataHipAnglesD1<- data.frame(apply(leftdataHipAnglesD1, 2, function(x) as.numeric(x)))
leftdataHipAnglesD1<-cbind(ID,leftdataHipAnglesD1)
rownames(leftdataHipAnglesD1)=1:44

# 2nd direction==Hip Add/Abd
leftdataHipAnglesD2 <-t(leftdata[leftdata$CurveName=="Hip Add/Abd",4:47]) # dim (44,51)
colnames(leftdataHipAnglesD2)=1:51
leftdataHipAnglesD2<- data.frame(apply(leftdataHipAnglesD2, 2, function(x) as.numeric(x)))
leftdataHipAnglesD2<-cbind(ID,leftdataHipAnglesD2)
rownames(leftdataHipAnglesD2)=1:44

# 3rd direction==Hip Int/Ext
leftdataHipAnglesD3 <-t(leftdata[leftdata$CurveName=="Hip Int/Ext",4:47]) # dim (44,51)
colnames(leftdataHipAnglesD3)=1:51
leftdataHipAnglesD3<- data.frame(apply(leftdataHipAnglesD3, 2, function(x) as.numeric(x)))
leftdataHipAnglesD3<-cbind(ID,leftdataHipAnglesD3)
rownames(leftdataHipAnglesD3)=1:44

#~~~~ Knee angle 
leftdataKneeAngles<-t(leftdata[leftdata$CurveName=="Knee Flx/Ext",4:47])   # dim (44,104)
colnames(leftdataKneeAngles)=1:51
leftdataKneeAngles<- data.frame(apply(leftdataKneeAngles, 2, function(x) as.numeric(x)))
leftdataKneeAngles<-cbind(ID,leftdataKneeAngles)
rownames(leftdataKneeAngles)=1:44

#~~~~ Ankle angle 
leftdataAnkleAngles <-t(leftdata[leftdata$CurveName=="Ankle Dor/Pla",4:47]) # dim (44,51)
colnames(leftdataAnkleAngles)=1:51
leftdataAnkleAngles<- data.frame(apply(leftdataAnkleAngles, 2, function(x) as.numeric(x)))
leftdataAnkleAngles<-cbind(ID,leftdataAnkleAngles)
rownames(leftdataAnkleAngles)=1:44

#~~~~ Foot angle 
leftdataFootProgressAngles<-t(leftdata[leftdata$CurveName=="Foot Int/Ext",4:47]) # dim (44,51)
colnames(leftdataFootProgressAngles)=1:51
leftdataFootProgressAngles<- data.frame(apply(leftdataFootProgressAngles, 2, function(x) as.numeric(x)))
leftdataFootProgressAngles<-cbind(ID,leftdataFootProgressAngles)
rownames(leftdataFootProgressAngles)=1:44


#~~~~~~~~~~~~ Right leg
rightdata<-data[data$Side=="R",]

#~~~~ Pelvis angle for three directions x,y and z

# 1st direction==Pelvis Ant/Pst
rightdataNewPelAnglesD1 <-t(rightdata[rightdata$CurveName=="Pelvis Ant/Pst",4:47]) # dim (44,51)
colnames(rightdataNewPelAnglesD1 )=1:51
rightdataNewPelAnglesD1<- data.frame(apply(rightdataNewPelAnglesD1, 2, function(x) as.numeric(x)))
rightdataNewPelAnglesD1<-cbind(ID,rightdataNewPelAnglesD1)
rownames(rightdataNewPelAnglesD1)=1:44

# 2nd direction==Pelvis Up/Dn
rightdataNewPelAnglesD2 <-t(rightdata[rightdata$CurveName=="Pelvic Up/Dn",4:47]) # dim (44,51)
colnames(rightdataNewPelAnglesD2)=1:51
rightdataNewPelAnglesD2<- data.frame(apply(rightdataNewPelAnglesD2, 2, function(x) as.numeric(x)))
rightdataNewPelAnglesD2<-cbind(ID,rightdataNewPelAnglesD2)
rownames(rightdataNewPelAnglesD2)=1:44

# 3rd direction==Pelvis Int/Ext
rightdataNewPelAnglesD3 <-t(rightdata[rightdata$CurveName=="Pelvic Int/Ext",4:47]) # dim (44,51)
colnames(rightdataNewPelAnglesD3)=1:51
rightdataNewPelAnglesD3<- data.frame(apply(rightdataNewPelAnglesD3, 2, function(x) as.numeric(x)))
rightdataNewPelAnglesD3<-cbind(ID,rightdataNewPelAnglesD3)
rownames(rightdataNewPelAnglesD3)=1:44

#~~~~ Hip angle for three directions x,y and z

# 1st direction==Hip Flx/Ext
rightdataHipAnglesD1 <-t(rightdata[rightdata$CurveName=="Hip Flx/Ext",4:47]) # dim (44,51)
colnames(rightdataHipAnglesD1)=1:51
rightdataHipAnglesD1<- data.frame(apply(rightdataHipAnglesD1, 2, function(x) as.numeric(x)))
rightdataHipAnglesD1<-cbind(ID,rightdataHipAnglesD1)
rownames(rightdataHipAnglesD1)=1:44

# 2nd direction==Hip Add/Abd
rightdataHipAnglesD2 <-t(rightdata[rightdata$CurveName=="Hip Add/Abd",4:47]) # dim (44,51)
colnames(rightdataHipAnglesD2)=1:51
rightdataHipAnglesD2<- data.frame(apply(rightdataHipAnglesD2, 2, function(x) as.numeric(x)))
rightdataHipAnglesD2<-cbind(ID,rightdataHipAnglesD2)
rownames(rightdataHipAnglesD2)=1:44

# 3rd direction==Hip Int/Ext
rightdataHipAnglesD3 <-t(rightdata[rightdata$CurveName=="Hip Int/Ext",4:47]) # dim (44,51)
colnames(rightdataHipAnglesD3)=1:51
rightdataHipAnglesD3<- data.frame(apply(rightdataHipAnglesD3, 2, function(x) as.numeric(x)))
rightdataHipAnglesD3<-cbind(ID,rightdataHipAnglesD3)
rownames(rightdataHipAnglesD3)=1:44

#~~~~ Knee angle 
rightdataKneeAngles<-t(rightdata[rightdata$CurveName=="Knee Flx/Ext",4:47])   # dim (44,104)
colnames(rightdataKneeAngles)=1:51
rightdataKneeAngles <- data.frame(apply(rightdataKneeAngles, 2, function(x) as.numeric(x)))
rightdataKneeAngles<-cbind(ID,rightdataKneeAngles)
rownames(rightdataKneeAngles)=1:44

#~~~~ Ankle angle 
rightdataAnkleAngles <-t(rightdata[rightdata$CurveName=="Ankle Dor/Pla",4:47]) # dim (44,51)
colnames(rightdataAnkleAngles)=1:51
rightdataAnkleAngles <- data.frame(apply(rightdataAnkleAngles, 2, function(x) as.numeric(x)))
rightdataAnkleAngles<-cbind(ID,rightdataAnkleAngles)
rownames(rightdataAnkleAngles)=1:44

#~~~~ Foot angle 
rightdataFootProgressAngles<-t(rightdata[rightdata$CurveName=="Foot Int/Ext",4:47]) # dim (44,51)
colnames(rightdataFootProgressAngles)=1:51
rightdataFootProgressAngles <- data.frame(apply(rightdataFootProgressAngles, 2, function(x) as.numeric(x)))
rightdataFootProgressAngles<-cbind(ID,rightdataFootProgressAngles)
rownames(rightdataFootProgressAngles)=1:44