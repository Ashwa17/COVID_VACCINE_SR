## Open Dataset

##Load Packages
library(meta)
library(metaviz)
library(metafor)

## Calculation of Raw Proportions and Log transformation
COVID_SR_Refusal$RawPR<-escalc(measure = "PR",xi=COVID_SR_Refusal$No,ni=COVID_SR_Refusal$N)
COVID_SR_Refusal$Trans<-escalc(measure = "PLO",xi=COVID_SR_Refusal$No,ni=COVID_SR_Refusal$N)

## Analysis model for Raw proportions : Forest plots and funnel plot
Model_PR<-rma(yi=COVID_SR_Refusal$Trans$yi,vi=COVID_SR_Refusal$Trans$vi, method = "REML")

##Analysis model corrected for skewness: Log transformed
Model_Trans<-rma(yi=COVID_SR_Refusal$Trans$yi,vi=COVID_SR_Refusal$Trans$vi,method = "REML")
transf.ilogit(Model_Trans$b)
transf.ilogit(Model_Trans$ci.lb)
transf.ilogit(Model_Trans$ci.ub)

## Forest plot 
Forest_plot<-viz_forest(Model_PR, study_labels = c(COVID_SR_Refusal$Author), xlab = "Raw Proportion", summary_label = "Pooled Proportion")

## Check for Publication Bias
leave_analysis<-leave1out(Model_Trans)
transf.ilogit(max(leave_analysis$estimate))
transf.ilogit(min(leave_analysis$estimate))
regtest(Model_Trans)
trimfill(Model_Trans)
predict(Trim, transf = transf.ilogit, digits = 4)

## Trim and fill and normal funnel plot
Funnel.trimfill=trimfill(Model_Trans)
Funnel_trim<-funnel(Funnel.trimfill,atransf = transf.ilogit,yaxis="sei",xlab ="Proporttion",digits = 2)
Funnel<-funnel(Model_PR,atransf = transf.ilogit,yaxis="sei",xlab ="Proporttion",digits = 2)

## Meta-regression with month as predictor and sub-group analysis for two half of the time frame
Time_continue<-rma(yi=COVID_SR_Refusal$Trans$yi,vi=COVID_SR_Refusal$Trans$vi, method = "REML", mods =COVID_SR_Refusal$Month_N)
Time_Cat<-rma(yi=COVID_SR_Refusal$Trans$yi,vi=COVID_SR_Refusal$Trans$vi, method = "REML", mods =COVID_SR_Refusal$Month_Cat)

## Correlation b/w time and intent options
cor.test(COVID_SR_Refusal$Intent,COVID_SR_Refusal$Month_N)

## Interaction b/w Intent and time
Time_response_interact<-rma(yi=COVID_SR_Refusal$Trans$yi,vi=COVID_SR_Refusal$Trans$vi, method = "REML", mods =COVID_SR_Refusal$Intent*COVID_SR_Refusal$Month_N)

## Sampling methods and intention
Study_design<-rma(yi=COVID_SR_Refusal$Trans$yi,vi=COVID_SR_Refusal$Trans$vi, method = "REML", mods =COVID_SR_Refusal$Stdy_dsgn)
Probablity<-subset(COVID_SR_Refusal,COVID_SR_Refusal$Stdy_dsgn==1)
Non_Probablity<-subset(COVID_SR_Refusal,COVID_SR_Refusal$Stdy_dsgn==3)

Probablity_Model<-rma(yi=Probablity$Trans$yi,vi=Probablity$Trans$vi)
Non_Probablity_Model<-rma(yi=Non_Probablity$Trans$yi,vi=Non_Probablity$Trans$vi)

transf.ilogit(Probablity_Model$b)
transf.ilogit(Probablity_Model$ci.lb)
transf.ilogit(Probablity_Model$ci.ub)

transf.ilogit(Non_Probablity_Model$b)
transf.ilogit(Non_Probablity_Model$ci.lb)
transf.ilogit(Non_Probablity_Model$ci.ub)

## Analysis of number of options vs intent to vaccinate.
Option_no<-rma(yi=COVID_SR_Refusal$Trans$yi,vi=COVID_SR_Refusal$Trans$vi, method = "REML", mods =COVID_SR_Refusal$Intent)
Options_two<-subset(COVID_SR_Refusal,COVID_SR_Refusal$Intent==0)
Options_three<-subset(COVID_SR_Refusal,COVID_SR_Refusal$Intent==1)

Options_two_Model<-rma(Options_two$Trans$yi, vi = Options_two$Trans$vi)
transf.ilogit(Options_two_Model$b)
transf.ilogit(Options_two_Model$ci.lb)
transf.ilogit(Options_two_Model$ci.ub)

Options_three_Model<-rma(Options_three$Trans$yi, vi = Options_three$Trans$vi)
transf.ilogit(Options_three_Model$b)
transf.ilogit(Options_three_Model$ci.lb)
transf.ilogit(Options_three_Model$ci.ub)

## Time trends for US,UK and China
UK_data<-subset(COVID_SR_Refusal,COVID_SR_Refusal$Country_ID==20)
Trans_UK<-rma(yi =UK_data$Trans$yi, vi =UK_data$Trans$vi, mods =UK_data$Trans$Month_N)

US_data<-subset(COVID_SR_Refusal,COVID_SR_Refusal$Country_ID==21)
Trans_US<-rma(yi =US_data$Trans$yi, vi = US_data$Trans$vi, mods =US_data$Month_N)

China_data<-subset(COVID_SR_Refusal,COVID_SR_Refusal$Country_ID==4)
Trans_China<-rma(yi = China_data$Trans$yi, vi = China_data$Trans$vi, mods =China_data$Month_N)

## ggPlot
library(ggplot2)
Plot_Time<-ggplot(COVID_SR_Refusal, aes(x =Month_N, y=Proportion))+geom_point(mapping=aes(color=Country,size=Total))+geom_smooth(method = "lm")

##DIFFERENCES IN VACCINE INTENTIONS BETWEEN CONTINENTS
Option_Continents<-rma(yi=COVID_SR_Refusal$Trans$yi,vi=COVID_SR_Refusal$Trans$vi, method = "REML", mods =COVID_SR_Refusal$Continent_code_N)
Option_Asia<-subset(COVID_SR_Refusal,COVID_SR_Refusal$Continent_code_N==1)
Option_Australia<-subset(COVID_SR_Refusal,COVID_SR_Refusal$Continent_code_N==2)
Option_Europe<-subset(COVID_SR_Refusal,COVID_SR_Refusal$Continent_code_N==3)
Option_North_Amarica<-subset(COVID_SR_Refusal,COVID_SR_Refusal$Continent_code_N==4)

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

Option_LMIC_HIC<-rma(yi=COVID_SR_Refusal$Trans$yi,vi=COVID_SR_Refusal$Trans$vi, method = "REML", mods =COVID_SR_Refusal$LMIC_HIC)
Option_HIC<-subset(COVID_SR_Refusal,COVID_SR_Refusal$LMIC_HIC==1)
Option_LMIC<-subset(COVID_SR_Refusal,COVID_SR_Refusal$LMIC_HIC==2)

Model_HIC<-rma(yi=Option_HIC$Trans$yi,vi=Option_HIC$Trans$vi)
transf.ilogit(Model_HIC$b)
transf.ilogit(Model_HIC$ci.lb)
transf.ilogit(Model_HIC$ci.ub)

Model_LMIC<-rma(yi=Option_LMIC$Trans$yi,vi=Option_LMIC$Trans$vi)
transf.ilogit(Model_LMIC$b)
transf.ilogit(Model_LMIC$ci.lb)
transf.ilogit(Model_LMIC$ci.ub)




  