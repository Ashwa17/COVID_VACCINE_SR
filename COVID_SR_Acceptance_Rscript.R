## Open Datatset
library(readxl)
COVID_SR_Acceptance <- read_excel("~/Library/Group Containers/3L68KQB4HG.group.com.readdle.smartemail/databases/messagesData/1/15787/COVID_SR_Acceptance.xlsx")
View(COVID_SR_Acceptance)

##Load Packages
library(meta)
library(metaviz)
library(metafor)

## Calculation of Raw Proportions and Log transformation
COVID_SR_Acceptance$RawPR<-escalc(measure = "PR",xi=COVID_SR_Acceptance$Yes,ni=COVID_SR_Acceptance$Total)
COVID_SR_Acceptance$Trans<-escalc(measure = "PLO",xi=COVID_SR_Acceptance$Yes,ni=COVID_SR_Acceptance$Total)

## Analysis model for Raw proportions : Forest plots and funnel plot
Model_PR<-rma(yi=COVID_SR_Acceptance$RawPR$yi,vi=COVID_SR_Acceptance$RawPR$vi,method = "REML")

##Analysis model corrected for skewness: Log transformed
Model_Trans<-rma(yi=COVID_SR_Acceptance$Trans$yi,vi=COVID_SR_Acceptance$Trans$vi,method = "REML")

##Back-transform the proportions
Back_trans<-transf.ilogit(Model_Trans$b) 
transf.ilogit(Model_Trans$ci.lb)
transf.ilogit(Model_Trans$ci.ub)

## Forest plot and funnel diagram
Forest_plot<-viz_forest(Model_PR, study_labels = c(COVID_SR_Acceptance$Study), xlab = "Raw Proportion", summary_label = "Pooled Proportion")
Funnel<-funnel(Model_PR, shade = "white", back = "white")

## Check for Publication Bias
leave_analysis<-leave1out(Model_Trans)
transf.ilogit(max(leave_analysis$estimate))
transf.ilogit(min(leave_analysis$estimate))
ranktest(Model_Trans)
trimfill(Model_Trans)

## Meta-regression with month as predictor and sub-group analysis for two halfs of the time frame
Time_continue<-rma(yi=COVID_SR_Acceptance$Trans$yi,vi=COVID_SR_Acceptance$Trans$vi, method = "REML", mods =COVID_SR_Acceptance$Month_N)
Time_Cat<-rma(yi=COVID_SR_Acceptance$Trans$yi,vi=COVID_SR_Acceptance$Trans$vi, method = "REML", mods =COVID_SR_Acceptance$Month_Cat)

## Correlation b/w time and intent options
cor.test(COVID_SR_Acceptance$Intent,COVID_SR_Acceptance$Month_N)

## Interaction b/w Intent and time
Time_response_interact<-rma(yi=COVID_SR_Acceptance$Trans$yi,vi=COVID_SR_Acceptance$Trans$vi, method = "REML", mods =COVID_SR_Acceptance$Intent*COVID_SR_Acceptance$Month_N)

## Sampling methods and intention
Study_design<-rma(yi=COVID_SR_Acceptance$Trans$yi,vi=COVID_SR_Acceptance$Trans$vi, method = "REML", mods =COVID_SR_Acceptance$Stdy_dsgn)
Probablity<-subset(COVID_SR_Acceptance,COVID_SR_Acceptance$Stdy_dsgn==1)
Non_Probablity<-subset(COVID_SR_Acceptance,COVID_SR_Acceptance$Stdy_dsgn==3)

Probablity_Model<-rma(yi=Probablity$Trans$yi,vi=Probablity$Trans$vi)
Non_Probablity_Model<-rma(yi=Non_Probablity$Trans$yi,vi=Non_Probablity$Trans$vi)

transf.ilogit(Probablity_Model$b)
transf.ilogit(Probablity_Model$ci.lb)
transf.ilogit(Probablity_Model$ci.ub)

transf.ilogit(Non_Probablity_Model$b)
transf.ilogit(Non_Probablity_Model$ci.lb)
transf.ilogit(Non_Probablity_Model$ci.ub)

## Analysis of number of options vs intent to vaccinate.
Option_no<-rma(yi=COVID_SR_Acceptance$Trans$yi,vi=COVID_SR_Acceptance$Trans$vi, method = "REML", mods =COVID_SR_Acceptance$Intent)
Options_two<-subset(COVID_SR_Acceptance,COVID_SR_Acceptance$Intent==0)
Options_three<-subset(COVID_SR_Acceptance,COVID_SR_Acceptance$Intent==1)

Options_two_Model<-rma(Options_two$Trans$yi, vi = Options_two$Trans$vi)
transf.ilogit(Options_two_Model$b)
transf.ilogit(Options_two_Model$ci.lb)
transf.ilogit(Options_two_Model$ci.ub)

Options_three_Model<-rma(Options_three$Trans$yi, vi = Options_three$Trans$vi)
transf.ilogit(Options_three_Model$b)
transf.ilogit(Options_three_Model$ci.lb)
transf.ilogit(Options_three_Model$ci.ub)

## Time treands for US,UK and China
UK_data<-subset(COVID_SR_Acceptance,COVID_SR_Acceptance$Country_code==20)
Trans_UK<-rma(yi = UK_data$Trans$yi, vi = UK_data$Trans$vi, mods =UK_data$Month_N)

US_data<-subset(COVID_SR_Acceptance,COVID_SR_Acceptance$Country_code==21)
Trans_US<-rma(yi = US_data$Trans$yi, vi = US_data$Trans$vi, mods =US_data$Month_N)

China_data<-subset(COVID_SR_Acceptance,COVID_SR_Acceptance$Country_code==4)
Trans_China<-rma(yi = China_data$Trans$yi, vi = China_data$Trans$vi, mods =China_data$Month_N)

## ggPlot
library(ggplot2)
Plot_Time<-ggplot(COVID_SR_Acceptance, aes(x =Month_N, y=PROP))+geom_point(mapping=aes(color=Country,size=Total))+geom_smooth(method = "lm")

##DIFFERENCES IN VACCINE INTENTIONS BETWEEN CONTINENTS

Option_Continents<-rma(yi=COVID_SR_Acceptance$Trans$yi,vi=COVID_SR_Acceptance$Trans$vi, method = "REML", mods =COVID_SR_Acceptance$Continent_code_N)
Option_Asia<-subset(COVID_SR_Acceptance,COVID_SR_Acceptance$Continent_code_N==1)<-subset(COVID_SR_Acceptance,COVID_SR_Acceptance$Continent_code_N==1)
Option_Australia<-subset(COVID_SR_Acceptance,COVID_SR_Acceptance$Continent_code_N==2)
Option_Europe<-subset(COVID_SR_Acceptance,COVID_SR_Acceptance$Continent_code_N==3)
Option_North_Amarica<-subset(COVID_SR_Acceptance,COVID_SR_Acceptance$Continent_code_N==4)

Model_Asia<-rma(yi=Option_Asia$Trans$yi,vi=Option_Asia$Trans$vi)
transf.ilogit(Model_Asia$b)
transf.ilogit(Model_Asia$ci.lb)
transf.ilogit(Model_Asia$ci.ub)

Model_Australia<-rma(yi=Option_Australia$Trans$yi,vi=Option_Australia$Trans$vi)
transf.ilogit(Model_Australia$b)
transf.ilogit(Model_Australia$ci.lb)
transf.ilogit(Model_Australia$ci.ub)

Model_Europe<-rma(yi=Option_Europe$Trans$yi,vi=Option_Europe$Trans$vi)
transf.ilogit(Model_Europe$b)
transf.ilogit(Model_Europe$ci.lb)
transf.ilogit(Model_Europe$ci.ub)

Model_North_Amarica<-rma(yi=Option_North_Amarica$Trans$yi,vi=Option_North_Amarica$Trans$vi)
transf.ilogit(Model_North_Amarica$b)
transf.ilogit(Model_North_Amarica$ci.lb)
transf.ilogit(Model_North_Amarica$ci.ub)

##DIFFERENCES IN VACCINE INTENTIONS BETWEEN LMICS AND HICS

Option_LMIC_HIC<-rma(yi=COVID_SR_Acceptance$Trans$yi,vi=COVID_SR_Acceptance$Trans$vi, method = "REML", mods =COVID_SR_Acceptance$LMIC_HIC)
Option_HIC<-subset(COVID_SR_Acceptance,COVID_SR_Acceptance$LMIC_HIC==1)
Option_LMIC<-subset(COVID_SR_Acceptance,COVID_SR_Acceptance$LMIC_HIC==2)

Model_HIC<-rma(yi=Option_HIC$Trans$yi,vi=Option_HIC$Trans$vi)
transf.ilogit(Model_HIC$b)
transf.ilogit(Model_HIC$ci.lb)
transf.ilogit(Model_HIC$ci.ub)

Model_LMIC<-rma(yi=Option_LMIC$Trans$yi,vi=Option_LMIC$Trans$vi)
transf.ilogit(Model_LMIC$b)
transf.ilogit(Model_LMIC$ci.lb)
transf.ilogit(Model_LMIC$ci.ub)


