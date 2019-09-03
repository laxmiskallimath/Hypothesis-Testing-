#Hypothesis Testing with One sample T-test - Example 1
# Exercise 1 : A company claims that on an average it takes only 40 hours to 
# process any purchase order. 
# Based on the data given below, can you validate the claim?
PO <- read.csv("E:/Laxmi/RCA/Stastics/Predictive Modeling/PM/Case Studies/Module 1/Exercise1_PO.csv")
View(PO)

# Step -1 State Hypothesis
# Ho: Average time taking for processing order is 40 hours (Mu=40)
# H1: Average time taking for processing order is not equal to 40 hours (Mu ??? 40)

# Step -2 Specify level of Significance
# Level of significance ?? =0.05

# Step -3 Use appropriate statistical test
# One sample t test
t.test(PO$Avg.time,mu=40)

# Step -4 Decision Rule
# p-value = 0.117 greater than alfa value so accept null hypothesis(P>0.05)

# Step -5 Conclusion
# Average time taking for processing order is equal to 40 hours. 
# Sample estimated mean is equal to 47.80

#################################################################################
# Assignment 1 : A  computer manufacturing company claims that on an average
# it will respond to any complaint logged by the customer from anywhere in 
# the world within 24 hours. Based on the data, validate the claim?
Response_time<- read.csv("E:/Laxmi/RCA/Stastics/Predictive Modeling/PM/Case Studies/Module 1/Assignment-1.csv")

# Step 1:- Sate the hypothesis
#H0:-average response time for any claim is 24hrs
#H1:-average response time for any claim not equal to 24hrs

# Step 2:-Specify level of significance -5%

# Use appropriate test 
# one sample t test 
t.test(Response_time$Response.Time,mu=24)

# Decision Rule 
# p value <0.05  reject null hypothesis 

# Conclusion
# Average response time for any claim is  not equal to 24 hrs 

###################################################################################
#Hypothesis Testing 2
# Exercise 2 : A super market chain has introduced a promotional activity 
# in its selected outlets in the city to increase the sales volume.
# Based on the data given below, check whether the promotional activity
# resulted in increasing the sales. The outlets where  promotional 
# activity introduced are denoted by 1 and others by 2?

# Running Independent t-test for Sales of outlet 1 and Outlet 2
Sales_outlet <- read.csv("E:/Laxmi/RCA/Stastics/Predictive Modeling/PM/Case Studies/Module 1/Exercise-2.csv")

# Step -1 State Hypothesis
# Ho: Average Sales volume is same in both outlet after advertisement  (Mu1=Mu2)
# H1: Average Sales volume is not same in both outlet after advertisement (Mu1 ??? Mu2)

# Step -2 Specify level of Significance
# Level of significance ?? =0.05

# Step -3 Use appropriate statistical test
# Independent T test
t.test(Sales_outlet$Sales~Sales_outlet$Outlet)
var.test(Sales_outlet$Sales~Sales_outlet$Outlet)
t.test(Sales_outlet$Sales~Sales_outlet$Outlet,Var.equal=T)

# Step-4 Decision rule Pvalue is greater than alpha value so we
# accept the null hypothesis which means Average sales volume is same 
# in both outlet even after advertisement,


# Step -5 Conclusion :- Promotional activity is not impacting on volume sales 

###################################################################################
# Assignment 2 : A bpo company have developed a new method for better
# utilization of its resources. 
# 10 observations on utilization from both methods is given below: Check whether
# the means utilization for both methods are same?
#Hypothesis Testing for comparing the best methods in the BPO Company
Method_BPO<- read.csv("E:/Laxmi/RCA/Stastics/Predictive Modeling/PM/Case Studies/Module 1/method.csv")
dim(Method_BPO)
str(Method_BPO)
Method_BPO$X <- NULL
Method_BPO$X.1 <- NULL
var.test(Method_BPO$Utilization~Method_BPO$Method)
t.test(Method_BPO$Utilization~Method_BPO$Method,var.equql=T)
#Decision:-p value is greater than alpha value ie p=0.8409>alpha=0.05

#Conclusion 
#We accept null hypothesis which means avg utilization for both methods old and new are same

###############################################################################
# Paired t-test
# Exercise 3 : Complete Analytics wants to assess if students who took up statistics course 
# have enhanced their knowledge after course completion. 
# The scores of the same set of students before and after course completion, 
# is listed.
CA_Marks<- read.csv("E:/Laxmi/RCA/Stastics/Predictive Modeling/PM/Case Studies/Module 1/ca_marks.csv")
# Step -1 State Hypothesis
# Ho: Average score of students is same before and after joining of complete Analytics(Mu1=Mu2)
# H1: Average score of students is not same before and after joining of complete Analytics(Mu1=Mu2)
# t (Mu1 ??? Mu2)

# Step -2 Specify level of Significance
# Level of significance ?? =0.05

# Step -3 Use appropriate statistical test
# Paired T test
t.test(CA_Marks$Before,CA_Marks$After,paired = T)
Before_mean <- mean(CA_Marks$Before)
After_mean <- mean(CA_Marks$After)
dbar <- Before_mean-After_mean
dbar

# Step 4:- Decision Rule :- p > alpha value ie p=0.07031
# Accept the null hypothesis 
# Average score of students is same before and after joining of complete
# Analytics(Mu1=Mu2)
# Here the mean of the differences (-1.3) should be between 
# the range of 95% confidence interval that is -2.7326995 to  0.1326995
# but here it is out of the range ie -1.3 
# Which tells that there is no impact of the course on the knowldge 
# before and after

# Step5 :-Conclusion 
# Average score is same before and after no impact of the course on student

#######
Advertis_tamerind <- read.csv("E:/Laxmi/RCA/Stastics/Predictive Modeling/PM/Case Studies/Module 1/Tamarindgarments.csv")
str(Advertis_tamerind)
summary(Advertis_tamerind)

# Step 1:-State hypothesis testing 
# HO:-The recorded ratings of tamarind brand garments before
# and after  an advertisement campaign are same (no effect of campaign)
# H1:-The recorded ratings of tamarind brand garments before and after 
# an advertisement campaign are not same (there is an effect of campaign brands)

# Step 2:-Level of significance -alpha =0.05 or 5%

# Step 3:-Use appropriate -
# Paired T test -both before and after variables are dependent on each other
options(scipen = 999)
t.test(Advertis_tamerind$Before,Advertis_tamerind$After,paired = T)

# Step4:-Decision - p = 0.000001084 < alpha =0.05,we reject null hypothesis 

# Step5:-Conclusion - 
# The recorded ratings of tamarind brand grarments before and after
# an advetisement campaign are not same which means there is definitley an
#impact is happening due to campaign (campaign of brands is effective )

#############################################################################
#Running ANOVA For Comparing More than 2 Independent Variables
# One way ANOVA 

# Exercise 4 Analyse if the sales of kitkat is affected by the area of display
# (storage location) in a store. Sales for a week is observed when kitkat is
# placed at the window side, counter side or on the shelf. 
kit_kat<- read.csv("E:/Laxmi/RCA/Stastics/Predictive Modeling/PM/Case Studies/Module 1/kit_kat.csv")
dim(kit_kat)o
str(kit_kat)
summary(kit_kat)

#Step1:-HO-in all the places avg sales is same 
# H1 -at least in one place sales  is differrent 

#Step2:-Specify level of significance alpha=5%

#Step3:- Use appropriate ttest 
#ANOVA Test
kit_kat$Place <- as.factor(kit_kat$Place)

str(kit_kat)

fit <- aov(kit_kat$Sales~kit_kat$Place)

summary(fit)

14235.714+5342.857

# tuckey test for post hoc anova -to get the analysis on all possible pair 
# comparision 
TukeyHSD(fit)
aggregate(kit_kat$Sales,by=list(kit_kat$Place),mean)
518.5714-509.2857

#Step 4:-Decision Rule
# p<alpha rejecting the null hypoyhesis since p value is less than alpha 

#step 5 :- conclusion 
# At least one of the place sales is different

#### Two way ANOVA 
kit_kat_2<- read.csv("E:/Laxmi/RCA/Stastics/Predictive Modeling/PM/Case Studies/Module 1/kitkat_2.csv")
str(kit_kat_2)
summary(kit_kat_2)

kit_kat_2$Place <- as.factor(kit_kat_2$Place)

kit_kat_2$Package.Type <- as.factor(kit_kat_2$Package.Type)

str(kit_kat_2)

kit_kat_2_fit <-aov(kit_kat_2$Sales~kit_kat_2$Place+kit_kat_2$Package.Type+kit_kat_2$Place*kit_kat_2$Package.Type)

summary(kit_kat_2_fit)

TukeyHSD(kit_kat_2_fit)
