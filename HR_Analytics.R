##==========================================================================================##
##                                                                                          ##
##   Project     : HR Analytics - Employee Attrition Case Study                             ##
##                                                                                          ##
##   Description : Build a logistic regression model to predict the probabilities of        ##
##                 employee attrition. Also, find the main factors which are important      ##
##                 factors for attrition and should be focused on right away by the         ##
##                 management.                                                              ##           
##                                                                                          ##
##   Date        : 25-Nov-2018                                                              ##
##                                                                                          ##
##   Author      : 1. Bhagyashree Barhate                                                   ##
##                 2. Deepankar Kotnala                                                     ##
##                 3. Gautami Ramesh Havele                                                 ##
##                 4. Rohit Saini                                                           ##
##                                                                                          ##
##==========================================================================================##

## Turning on the warnings
options(warn = 0)

## Installing the required packages

# install.packages("lubridate")
# install.packages("dplyr")
# install.packages("MASS")
# install.packages("car")
# install.packages("Rcpp")
# install.packages("rlang")
# install.packages("e1071")
# install.packages("caret")
# install.packages("ROCR")
# install.packages("GGally")
# install.packages("ggplot2")
# install.packages("formattable")
# install.packages("cowplot")
# install.packages("caTools")
# install.packages("GGally")
# install.packages("corr")

## Loading the required libraries
library(Rcpp)
library(rlang)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(corrplot)
library(MASS)
library(cowplot)
library(caTools)
library(GGally)
library(lubridate)
library(dplyr)
library(formattable)
library(Hmisc)
library(ROCR)
# Suppressing warnings
options(warn=-1)

##==========================================================================================##
##                                                                                          ##
##                                  D A T A   L O A D I N G                                 ##
##                                                                                          ##
##==========================================================================================##

employee_df   <- read.csv("employee_survey_data.csv",stringsAsFactors = F)
general_df    <- read.csv("general_data.csv",stringsAsFactors = F)
mgr_survey_df <- read.csv("manager_survey_data.csv",stringsAsFactors = F)
in_time_df    <- read.csv("in_time.csv",stringsAsFactors = F)
out_time_df   <- read.csv("out_time.csv",stringsAsFactors = F)

# All 5 data files have been loaded into different data frames.

##==========================================================================================##
##                           O B S E R V I N G   T H E   D A T A                            ##
##==========================================================================================##

str(employee_df)
str(general_df)
str(mgr_survey_df)
#str(in_time_df)
#str(out_time_df)

# Changing the first column name in in_time_df and out_time_df to "EmployeeID"

colnames(in_time_df)[1]  <- "EmployeeID"
colnames(out_time_df)[1] <- "EmployeeID"

# Checking the count of unique employee information present in each data file

length(unique((employee_df$EmployeeID)))    #4410
length(unique((general_df$EmployeeID)))     #4410
length(unique((mgr_survey_df$EmployeeID)))  #4410
length(unique((in_time_df$EmployeeID)))     #4410
length(unique((out_time_df$EmployeeID)))    #4410

# So now we know that there are exactly same number of employee details present in all the data files.
# Let's check whether all the employee Ids match across all the data files.

setdiff(employee_df$EmployeeID,general_df$EmployeeID)
setdiff(employee_df$EmployeeID,mgr_survey_df$EmployeeID)
setdiff(employee_df$EmployeeID,in_time_df$EmployeeID)
setdiff(employee_df$EmployeeID,out_time_df$EmployeeID)
# Since all the employee IDs are same in all the data frames,
# we are going to merge them directly into a master data frame

## Merging the Data frames into a Master Data Frame

master_df <- merge(employee_df, general_df, by = "EmployeeID")
master_df <- merge(master_df, mgr_survey_df, by = "EmployeeID")


##==========================================================================================##
##                                                                                          ##
##                E X P L O R A T O R Y   D A T A   A N A L Y S I S   ( E D A )             ##
##                                                                                          ##
##==========================================================================================##

## Removing Public Holidays
names(employee_df)
# REMOVING PUBLIC HOLIDAYS FROM IN_TIME_DF
public_holidays     <- colSums(is.na(in_time_df)) == nrow(in_time_df)
public_holiday_list <- names(in_time_df)[public_holidays]
public_holiday_list
# [1] "X2015.01.01" "X2015.01.14" "X2015.01.26" "X2015.03.05" "X2015.05.01" "X2015.07.17"
# [7] "X2015.09.17" "X2015.10.02" "X2015.11.09" "X2015.11.10" "X2015.11.11" "X2015.12.25"

# Removing these days from in_time_df
in_time_df = subset(in_time_df, select = -c(X2015.01.01,X2015.01.14,X2015.01.26,X2015.03.05,X2015.05.01,X2015.07.17,X2015.09.17,X2015.10.02,X2015.11.09,X2015.11.10,X2015.11.11,X2015.12.25))

# REMOVING PUBLIC HOLIDAYS FROM OUT_TIME_DF. 
# THESE SHOULD BE SAME AS PRESENT IN IN_TIME_DF.
public_holidays     <- colSums(is.na(out_time_df)) == nrow(out_time_df)
public_holiday_list <- names(out_time_df)[public_holidays]
public_holiday_list
# [1] "X2015.01.01" "X2015.01.14" "X2015.01.26" "X2015.03.05" "X2015.05.01" "X2015.07.17"
# [7] "X2015.09.17" "X2015.10.02" "X2015.11.09" "X2015.11.10" "X2015.11.11" "X2015.12.25"

# Removing these days from out_time_df
out_time_df = subset(out_time_df, select = -c(X2015.01.01,X2015.01.14,X2015.01.26,X2015.03.05,X2015.05.01,X2015.07.17,X2015.09.17,X2015.10.02,X2015.11.09,X2015.11.10,X2015.11.11,X2015.12.25))

##==========================================================================================##
## Calculating the leaves taken per employee - Derived Variable
##==========================================================================================##

# Checking whether there are same number of NA observations on same days. 
# Here we are checking for data consistency in in_time_df and out_time_df

leaves_per_employee <- as.data.frame(apply(in_time_df, 1, function(x) sum(is.na(x))))
leaves_per_employee$out_time_na <- as.data.frame(apply(out_time_df, 1, function(x) sum(is.na(x))))
colnames(leaves_per_employee) <- c("in_time_na","out_time_na")

leaves_per_employee$match <- ifelse(leaves_per_employee$in_time_na==leaves_per_employee$out_time_na,1,NA)
length(is.na(leaves_per_employee$match))

# That means the in_time and out_time for each employee are consistent.
# There are no inconsistencies like only in_time is present, or just out_time is present and the in_time is absent.
# So now, we can go ahead and merge the leaves per employee to the master_df.

leaves <- as.data.frame(leaves_per_employee$in_time_na)
colnames(leaves) <- "TotalLeaves"

master_df <- cbind(master_df,leaves)

##==========================================================================================##
## Calculating the average working hours per employee - Derived Variable
##==========================================================================================##

# Checking the class of date and time values in data frame
class(in_time_df[,3])
# character type

# Since POSIXlt is slower, we are going to use strptime (because the dates are in character format)
# POSIXlt will try to be intelligent by first checking the datatype and then converting it into date format,
# strptime will directly convert character values to dates, and hence it is faster.

# in_time <- as.data.frame(sapply(in_time_data,function(x) as.POSIXlt(x, origin="1970-01-01","%Y-%m-%d %H:%M:%S")))

in_time  <- as.data.frame(sapply(in_time_df[,-1],function(x) strptime(x,"%Y-%m-%d %H:%M:%S")))
out_time <- as.data.frame(sapply(out_time_df[,-1],function(x) strptime(x,"%Y-%m-%d %H:%M:%S")))

length(which(is.na(in_time)))
length(which(is.na(in_time_df)))
# No new NA values introduced after the conversion.

# Calculating the daily working hours for each employee.
daily_working_hours <- out_time - in_time

# Changing the datatype of each value to double for applying mean function (taking average) in the next step.
daily_working_hours <- sapply(daily_working_hours, function(x) x<- as.double(x))

# Calculating the average working hours for each employee
avg_working_hours <- as.data.frame(round(apply(daily_working_hours, 1, mean, na.rm=T),2))
colnames(avg_working_hours) <- "AvgWorkHours"

# Adding average_working_hours to master_df (master dataframe)
master_df <- cbind(master_df,avg_working_hours)

##==========================================================================================##
##                   C H E C K I N G   F O R   N A   O B S E R V A T I O N S                ##
##                                                                                          ##
##                               I N   T H E   D A T A F R A M E                            ##
##==========================================================================================##

sapply(master_df, function(x) 
  length(which(is.na(x))))

#             EmployeeID EnvironmentSatisfaction         JobSatisfaction         WorkLifeBalance 
#                      0                      25                      20                      38 
#                    Age               Attrition          BusinessTravel              Department 
#                      0                       0                       0                       0 
#       DistanceFromHome               Education          EducationField           EmployeeCount 
#                      0                       0                       0                       0 
#                 Gender                JobLevel                 JobRole           MaritalStatus 
#                      0                       0                       0                       0 
#          MonthlyIncome      NumCompaniesWorked                  Over18       PercentSalaryHike 
#                      0                      19                       0                       0 
#          StandardHours        StockOptionLevel       TotalWorkingYears   TrainingTimesLastYear 
#                      0                       0                       9                       0 
#         YearsAtCompany YearsSinceLastPromotion    YearsWithCurrManager          JobInvolvement 
#                      0                       0                       0                       0 
#      PerformanceRating            AvgWorkHours 
#                      0                       0 

# Replacing the NAs in EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance 
# with the median of these respective columns.
master_df[which(is.na(master_df$EnvironmentSatisfaction)),"EnvironmentSatisfaction"] <- 
                                                  median(master_df$EnvironmentSatisfaction,na.rm=T)

master_df[which(is.na(master_df$JobSatisfaction)),"JobSatisfaction"] <- 
                                                  median(master_df$JobSatisfaction,na.rm=T)
master_df[which(is.na(master_df$WorkLifeBalance)),"WorkLifeBalance"] <- 
                                                  median(master_df$WorkLifeBalance,na.rm=T)

# Missing values in the above three columns have been imputed by substituting the 
# respective column median values.

# Taking median for NumCompaniesWorked (19 NA observations) and 
# TotalWorkingYears (9 NA observations) would not be the right choice.
# (19 + 9)/4410 = 0.006349206 i.e, less than 0.01 % observations are having 
# NumCompaniesWorked and TotalWorkingYears as NA.
# So we can remove these observations from the analysis.

master_df <- master_df[!is.na(master_df$NumCompaniesWorked),]
master_df <- master_df[!is.na(master_df$TotalWorkingYears),]



##====================================================================================##
##        Checking for columns with redundant data (i.e, repetitive values)           ##
##====================================================================================##

sapply(master_df, function(x) length(unique(x)))
str(master_df)
# Columns like EmployeeCount, Over18, StandardHours are having only 1 repetitive value.
# Standardhours are 8 for each employee.
# All the employees are over 18 in age.
# These kind of values are not required in our analysis part, 
# and hence we are removing these columns.
# We will use them explicitly if required.

master_df <- master_df[,-c(12,19,21)]

# Replacing the numeric values in Education column with their actual values.
# These values are taken from the data dictionary provided to us.

master_df$Education[master_df$Education == 1] <- "Below College"
master_df$Education[master_df$Education == 2] <- "College"
master_df$Education[master_df$Education == 3] <- "Bachelor"
master_df$Education[master_df$Education == 4] <- "Master"
master_df$Education[master_df$Education == 5] <- "Doctor"

# Replacing the numeric values in EnvironmentSatisfaction column with their actual values.

master_df$EnvironmentSatisfaction[master_df$EnvironmentSatisfaction == 1] <- "Low"
master_df$EnvironmentSatisfaction[master_df$EnvironmentSatisfaction == 2] <- "Medium"
master_df$EnvironmentSatisfaction[master_df$EnvironmentSatisfaction == 3] <- "High"
master_df$EnvironmentSatisfaction[master_df$EnvironmentSatisfaction == 4] <- "Very High"

# Replacing the numeric values in JobInvolvement column with their actual values.

master_df$JobInvolvement[master_df$JobInvolvement == 1] <- "Low"
master_df$JobInvolvement[master_df$JobInvolvement == 2] <- "Medium"
master_df$JobInvolvement[master_df$JobInvolvement == 3] <- "High"
master_df$JobInvolvement[master_df$JobInvolvement == 4] <- "Very High"

# Replacing the numeric values in JobSatisfaction column with their actual values.

master_df$JobSatisfaction[master_df$JobSatisfaction == 1] <- "Low"
master_df$JobSatisfaction[master_df$JobSatisfaction == 2] <- "Medium"
master_df$JobSatisfaction[master_df$JobSatisfaction == 3] <- "High"
master_df$JobSatisfaction[master_df$JobSatisfaction == 4] <- "Very High"


# Replacing the numeric values in PerformanceRating column with their actual values.

master_df$PerformanceRating[master_df$PerformanceRating == 1] <- "Low"
master_df$PerformanceRating[master_df$PerformanceRating == 2] <- "Good"
master_df$PerformanceRating[master_df$PerformanceRating == 3] <- "Excellent"
master_df$PerformanceRating[master_df$PerformanceRating == 4] <- "Outstanding"


# Replacing the numeric values in WorkLifeBalance column with their actual values.

master_df$WorkLifeBalance[master_df$WorkLifeBalance == 1] <- "Bad"
master_df$WorkLifeBalance[master_df$WorkLifeBalance == 2] <- "Good"
master_df$WorkLifeBalance[master_df$WorkLifeBalance == 3] <- "Better"
master_df$WorkLifeBalance[master_df$WorkLifeBalance == 4] <- "Best"

# Converting JobLevel to factor
master_df$JobLevel <- as.factor(master_df$JobLevel)

# Removing EmployeeID before building the model from master_df
master_df <- master_df[,-1]

##====================================================================================##
##                                                                                    ##
##                      U N I V A R I A T E   A N A L Y S I S                         ##
##                                                                                    ##
##====================================================================================##

# Converting target variable Attrition from Yes/No character to factorwith levels 1/0 
master_df$Attrition <- ifelse(master_df$Attrition=="Yes",1,0)

# Checking the Attrition rate of prospect employee
Attrition <- (sum(master_df$Attrition)/nrow(master_df)) * 100
Attrition
# 16.09 % attrition rate

# Setting a theme for the plots

plot_theme <-   theme_light() +
  theme(axis.text = element_text(size =12)) + theme(
    plot.title = element_text(size=10, face="bold"),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12)
  )


univariate_Analysis_Plot <- function(var, yAxisTitle) {
  ggplot(master_df, aes(x = "", y = var)) +
    geom_boxplot(fill = "khaki1", outlier.colour = "red", outlier.shape = 1, 
                 outlier.size = 4, width = 0.4) +
    stat_boxplot(geom = "errorbar", width = 0.3) +
    labs( y = yAxisTitle, title = paste(yAxisTitle, "Plot")) +
    plot_theme }

# Plot a grid of all the Boxplots.
plot_grid(
univariate_Analysis_Plot(master_df$Avg, "Emp Avg Hours"),
univariate_Analysis_Plot(master_df$Age, "Employee Age"),
univariate_Analysis_Plot(master_df$DistanceFromHome, "Distance From Home"),
univariate_Analysis_Plot(master_df$MonthlyIncome, "Monthly Income"),
univariate_Analysis_Plot(master_df$NumCompaniesWorked, "Num of Companies Worked"),
univariate_Analysis_Plot(master_df$PercentSalaryHike, "Percent Salary Hike"),
univariate_Analysis_Plot(master_df$TotalWorkingYears, "Total Working Years"),
univariate_Analysis_Plot(master_df$TrainingTimesLastYear, "Training Times Last Year"),
univariate_Analysis_Plot(master_df$YearsAtCompany, "Years At Company"),
univariate_Analysis_Plot(master_df$YearsSinceLastPromotion, "YearsSinceLastPromotion"),
univariate_Analysis_Plot(master_df$YearsWithCurrManager, "Years With Current Manager")
)

## There are outliers present in AvgWorkHours, MonthlyIncome, NumCompaniesWorked, 
## TotalWorkingYears, TrainingTimesLastYear, YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrManager
## These outliers are required to be treated. We will cap these outliers.

##====================================================================================##
##                              T R E A T I N G   O U T L I E R S                     ##
##====================================================================================##
## Function to treat outliers
outlierFun <- function(x){
  if(length(boxplot.stats(x)$out) > 0){
    x[which(x %in% boxplot.stats(x)$out)] <- boxplot.stats(x)$stats[5]
    return(x)
  }
  else{
    return(x)  
  }
}

##====================================================================================##
## Applying function to treat outliers (capping)                                      ##                                          
##====================================================================================##
master_df$Age                       <- outlierFun(master_df$Age)                    
master_df$DistanceFromHome          <- outlierFun(master_df$DistanceFromHome)       
master_df$MonthlyIncome             <- outlierFun(master_df$MonthlyIncome)          
master_df$NumCompaniesWorked        <- outlierFun(master_df$NumCompaniesWorked)     
master_df$PercentSalaryHike         <- outlierFun(master_df$PercentSalaryHike)      
master_df$StockOptionLevel          <- outlierFun(master_df$StockOptionLevel)       
master_df$TotalWorkingYears         <- outlierFun(master_df$TotalWorkingYears)     
master_df$TrainingTimesLastYear     <- outlierFun(master_df$TrainingTimesLastYear)  
master_df$YearsAtCompany            <- outlierFun(master_df$YearsAtCompany)         
master_df$YearsSinceLastPromotion   <- outlierFun(master_df$YearsSinceLastPromotion)
master_df$YearsWithCurrManager      <- outlierFun(master_df$YearsWithCurrManager)   
master_df$AvgWorkHours              <- outlierFun(master_df$AvgWorkHours)           

# Check whether outliers have been treated or not.

plot_grid(
univariate_Analysis_Plot(master_df$Avg, "Employee Avgerage Hours"),
univariate_Analysis_Plot(master_df$Age, "Employee Age"),
univariate_Analysis_Plot(master_df$DistanceFromHome, "Distance From Home"),
univariate_Analysis_Plot(master_df$MonthlyIncome, "Monthly Income"),
univariate_Analysis_Plot(master_df$NumCompaniesWorked, "Number of Companies Worked"),
univariate_Analysis_Plot(master_df$PercentSalaryHike, "Percent Salary Hike"),
univariate_Analysis_Plot(master_df$TotalWorkingYears, "Total Working Years"),
univariate_Analysis_Plot(master_df$TrainingTimesLastYear, "Training Times Last Year"),
univariate_Analysis_Plot(master_df$YearsAtCompany, "Years At Company"),
univariate_Analysis_Plot(master_df$YearsSinceLastPromotion, "Years Since Last Promotion"),
univariate_Analysis_Plot(master_df$YearsWithCurrManager, "Years With Current Manager")
)
# All outliers have been treated


##====================================================================================##
##                                                                                    ##
##                   M U L T I V A R I A T E   A N A L Y S I S                        ##
##                                                                                    ##
##====================================================================================##

# Convert the type of columns having categorical variables to character
master_df$Education               <-  as.character(master_df$Education)
master_df$JobLevel                <-  as.character(master_df$JobLevel)
master_df$StockOptionLevel        <-  as.character(master_df$StockOptionLevel)
master_df$EnvironmentSatisfaction <-  as.character(master_df$EnvironmentSatisfaction)
master_df$JobSatisfaction         <-  as.character(master_df$JobSatisfaction)
master_df$WorkLifeBalance         <-  as.character(master_df$WorkLifeBalance)
master_df$JobInvolvement          <-  as.character(master_df$JobInvolvement)
master_df$PerformanceRating       <-  as.character(master_df$PerformanceRating)

# Create a dataframe for numeric variables
num_var_df <- Filter(is.numeric, master_df)

# Create a dataframe for Categorical variables 
cat_var_df <- Filter(is.character, master_df)

##====================================================================================##
# Plotting a correlation matrix for the numeric data.
##====================================================================================##

library(RColorBrewer)
corMat <- cor(num_var_df)

col1 <- colorRampPalette(brewer.pal(10,"BrBG"))

corrplot(corMat, method="number", type="lower", 
         tl.col="black", tl.srt=45,order="hclust", tl.cex = 0.75, 
         sig.level = 0.05, insig = "pch", pch.cex = 1, col = col1(100) 
        )

# Findings from the correlation matrix:
# 1. Attrition is negatively correlated to:
#    a. Age
#    b. Total working years
#    c. Years at company
#    d. Years with current manager
#    e. Total Leaves

#  This means that:
#  The number of younger employees leaving the organization is high.
#  The employees who work more with one manager are less likely to leave the organization
#  The employees who take less leaves( or who get less leaves approved) are the ones who leave the organization more often.


# 2. YearsWithCurrManager is highly correlated to YearsAtCompany.
#    That means, employees who are in the organisation since a long time are 
#    mostly those who are working for the same manager for most of the years of their service.

# 3. YearsAtCompany is highly correlated to YearsSinceLastPromotion.
#    That means, employees who are working in the organisation since a long term have not received a promotion since a long term.
#    This also means that fresh/new joinee employees have recently received their promotion.

# 4. Age and total working years are strongly correlated.
# 

##====================================================================================##
# Creating a generalised function for Continuous Multivariate Analysis.
##====================================================================================##
contMultivarFun <- function(xvar, yvar, xlab, ylab) {
                   ggplot(master_df, aes(x = xvar, y = yvar, fill = xvar)) +
                   geom_boxplot(outlier.colour = "red", outlier.shape = 1,outlier.size = 3, show.legend = F) +
                   stat_boxplot(geom = "errorbar", width = 0.5) +
                   labs(x = xlab, y = ylab, title = paste("Plot of",xlab, "vs", ylab)) +
                   plot_theme
}

# Let's see what effect these Continuous variables have on the employee attrition 
# and find out whether any of these is a significant factor leading to employee attrition.

# AvgWorkHours, TotalLeaves, YearsWithCurrManager, YearsSinceLastPromotion, YearsAtCompany
# TrainingTimeLastYear, TotalWorkingYears, PercentSalaryHike, NumCompaniesWorked, MonthlyIncome, 
# DistanceFromHome, Age

plot_grid(
contMultivarFun(master_df$Attrition,master_df$AvgWorkHours,"Attrition","Average Working Hours"),
contMultivarFun(master_df$Attrition,master_df$TotalLeaves,"Attrition","Total Leaves"),
contMultivarFun(master_df$Attrition,master_df$YearsWithCurrManager,"Attrition","Years With Current Manager"),
contMultivarFun(master_df$Attrition,master_df$YearsSinceLastPromotion,"Attrition","Years Since Last Promotion")
)
plot_grid(
contMultivarFun(master_df$Attrition,master_df$YearsAtCompany,"Attrition","Years At Company"),
contMultivarFun(master_df$Attrition,master_df$TrainingTimesLastYear,"Attrition","Training Time Last Year"),
contMultivarFun(master_df$Attrition,master_df$TotalWorkingYears,"Attrition","Total Working Years"),
contMultivarFun(master_df$Attrition,master_df$PercentSalaryHike,"Attrition","Percent Salary Hike")
)
plot_grid(
contMultivarFun(master_df$Attrition,master_df$NumCompaniesWorked,"Attrition","Num Companies Worked"),
contMultivarFun(master_df$Attrition,master_df$MonthlyIncome,"Attrition","Monthly Income"),
contMultivarFun(master_df$Attrition,master_df$DistanceFromHome,"Attrition","Distance From Home"),
contMultivarFun(master_df$Attrition,master_df$Age,"Attrition","Age")
)


##====================================================================================##
# Create a generalised function to perform bivariate analysis                     
##====================================================================================##
BiVariateFun <- function(xval,yval,xlab,ylab){
  as.data.frame(percent(prop.table(table(yval, xval), 2))) %>%
    ggplot(aes(x=xval,y=Freq,fill=yval))+
    geom_col(colour = "black",position = "fill")+ 
    geom_text(aes(label = Freq),
              position = position_fill(vjust = .5), 
              size = 4) +
    labs(fill=ylab,x=xlab,y="Proportion", 
         title = paste0(ylab," vs ", xlab))+
    plot_theme
}


# Getting the names of categorical variables 
names(Filter(is.character, master_df))

# We will apply this function on below variables and plot their relationshio with employee Attrition.
# "EnvironmentSatisfaction" "JobSatisfaction"         "WorkLifeBalance"         "Attrition"               "BusinessTravel"         
# "Department"              "Education"               "EducationField"          "Gender"                  "JobRole"                
# "MaritalStatus"           "JobInvolvement"          "PerformanceRating"  

# Plotting grids of different plots. (Grouping of plots is done here, just for presentation purpose)
plot_grid(
BiVariateFun(master_df$EnvironmentSatisfaction,master_df$Attrition, "Environment Satisfaction", "Attrition") ,
BiVariateFun( master_df$JobSatisfaction,master_df$Attrition,
               "Job Satisfaction", "Attrition") ,
BiVariateFun( master_df$WorkLifeBalance,master_df$Attrition,
               "Work-Life Balance", "Attrition") ,
BiVariateFun( master_df$Education,master_df$Attrition,
               "Education", "Attrition") + theme(axis.text = element_text(size =12)) 
)


plot_grid(
BiVariateFun( master_df$Department,master_df$Attrition,
               "Department", "Attrition")  + theme(axis.text.x = element_text(angle=45, hjust=1,size =11)),
BiVariateFun( master_df$BusinessTravel,master_df$Attrition,
              "Business Travel", "Attrition") + theme(axis.text.x = element_text(angle=45, hjust=1))
)

plot_grid(
BiVariateFun( master_df$EducationField,master_df$Attrition,
               "Education Field", "Attrition") + theme(axis.text.x = element_text(angle=45, hjust=1)),
BiVariateFun( master_df$JobRole,master_df$Attrition,
               "JobRole", "Attrition") + theme(axis.text.x = element_text(angle=45, hjust=1))
)


plot_grid(
BiVariateFun( master_df$Gender,master_df$Attrition,
               "Gender", "Attrition") ,
BiVariateFun( master_df$JobInvolvement,master_df$Attrition,
               "Job Involvement", "Attrition") ,
BiVariateFun( master_df$MaritalStatus,master_df$Attrition,
               "Marital Status", "Attrition") ,
BiVariateFun( master_df$PerformanceRating,master_df$Attrition,
               "Performance Rating", "Attrition")
)

##====================================================================================##
##                              Feature Standardisation                               ##
##                         S C A L I N G    V A R I A B L E S                         ##
##====================================================================================##

# Normalising continuous features
str(master_df)

master_df$Age                       <- scale(master_df$Age)
master_df$DistanceFromHome          <- scale(master_df$DistanceFromHome)
master_df$MonthlyIncome             <- scale(master_df$MonthlyIncome)
master_df$NumCompaniesWorked        <- scale(master_df$NumCompaniesWorked)
master_df$PercentSalaryHike         <- scale(master_df$PercentSalaryHike)
master_df$TotalWorkingYears         <- scale(master_df$TotalWorkingYears)
master_df$TrainingTimesLastYear     <- scale(master_df$TrainingTimesLastYear)
master_df$YearsAtCompany            <- scale(master_df$YearsAtCompany)
master_df$YearsSinceLastPromotion   <- scale(master_df$YearsSinceLastPromotion)
master_df$YearsWithCurrManager      <- scale(master_df$YearsWithCurrManager)

##====================================================================================##
## Creating dummy variables for factor attributes
##====================================================================================##
# Using the cat_var_df data frame which we have created earlier.
# This data frame contains data of categorical variables only.

dummies<- data.frame(sapply(cat_var_df, 
          function(x) data.frame(model.matrix(~x-1,data =master_df))[,-1]))

# num_var_df was created earlier in which data for only numeric variables is present.
master_final_df <- cbind(num_var_df,dummies)

# Removing Leaves data from the data frame. 
# We are not going to include that variable in our model.
master_final_df <- master_final_df[,-12] 

##====================================================================================##
##                                                                                    ##
##                             M O D E L   C R E A T I O N                            ##
##                                                                                    ##
##====================================================================================##

# Splitting the Data into Train and Test Data

set.seed(1000)

# Here we are splitting the data into train and test data. 
# 70% of the data will be used to train the model.
# The remaining 30% data will be used to test the model.
indices = sample.split(master_final_df$Attrition, SplitRatio = 0.7)

# Getting the Training data
train = master_final_df[indices,]
# Getting the Testing data
test = master_final_df[!(indices),]

# Building the model.

mod_1 = glm(Attrition ~., data = train, family = "binomial")

summary(mod_1)
# AIC: 2107
# Null deviance: 2704.5  on 3066  degrees of freedom
# Residual deviance: 1995.0  on 3011  degrees of freedom

# Stepwise Selection - using StepAIC

# library MASS will be required here. 
# We have already loaded this library in the initial phase of this code.
mod_2<- stepAIC(mod_1, direction="both")

summary(mod_2)
# AIC: 2082.9

# Removing the multicollinear variables using vif. 
# We would require 'car' library for vif which is already loaded in the initial part of this code.
vif(mod_2)


mod_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               AvgWorkHours + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
               JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
               WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.xDoctor + EducationField.xOther + 
               JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + StockOptionLevel.x1 + JobInvolvement.xLow + 
               JobInvolvement.xMedium + JobInvolvement.xVery.High, family = "binomial", 
             data = train)

summary(mod_3)
vif(mod_3)

## Removing EducationField.xOther

mod_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               AvgWorkHours + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
               JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
               WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.xDoctor + 
               JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + StockOptionLevel.x1 + JobInvolvement.xLow + 
               JobInvolvement.xMedium + JobInvolvement.xVery.High, family = "binomial", 
             data = train)

summary(mod_4)
vif(mod_4)

## Removing JobLevel.x5 
#  p value = 0.0547

mod_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               AvgWorkHours + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
               JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
               WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.xDoctor + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + StockOptionLevel.x1 + JobInvolvement.xLow + 
               JobInvolvement.xMedium + JobInvolvement.xVery.High, family = "binomial", 
             data = train)

summary(mod_5)
vif(mod_5)

## Removing Education.xDoctor
#  p value = 0.058095

mod_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               AvgWorkHours + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
               JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
               WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales +JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + StockOptionLevel.x1 + JobInvolvement.xLow + 
               JobInvolvement.xMedium + JobInvolvement.xVery.High, family = "binomial", 
             data = train)

summary(mod_6)
vif(mod_6)

## Removing StockOptionLevel.x1
#  p value = 0.046607

mod_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               AvgWorkHours + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
               JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
               WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales +JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + JobInvolvement.xLow + 
               JobInvolvement.xMedium + JobInvolvement.xVery.High, family = "binomial", 
             data = train)

summary(mod_7)
vif(mod_7)

## Removing JobRole.xResearch.Scientist
#  p value = 0.044475

mod_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               AvgWorkHours + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
               JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
               WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales +JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + JobInvolvement.xLow + 
               JobInvolvement.xMedium + JobInvolvement.xVery.High, family = "binomial", 
             data = train)

summary(mod_8)
vif(mod_8)

## Removing EnvironmentSatisfaction.xVery.High
#  p value = 0.037599

mod_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               AvgWorkHours + EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +
               JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
               WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales +JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + JobInvolvement.xLow + 
               JobInvolvement.xMedium + JobInvolvement.xVery.High, family = "binomial", 
             data = train)

summary(mod_9)
vif(mod_9)

## Removing TrainingTimesLastYear
#  p value = 0.024970

mod_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales +JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + JobInvolvement.xLow + 
                JobInvolvement.xMedium + JobInvolvement.xVery.High, family = "binomial", 
              data = train)

summary(mod_10)
vif(mod_10)

## Removing JobInvolvement.xVery.High 
#  p value = 0.015713

mod_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales +JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + JobInvolvement.xLow + 
                JobInvolvement.xMedium ,family = "binomial", 
              data = train)

summary(mod_11)
vif(mod_11)

## Removing JobInvolvement.xLow 
#  p value = 0.013837

mod_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales +JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                JobInvolvement.xMedium ,family = "binomial", 
              data = train)

summary(mod_12)
vif(mod_12)

## Removing JobRole.xSales.Executive 
#  p value = 0.009028

mod_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales +JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                MaritalStatus.xSingle + 
                JobInvolvement.xMedium ,family = "binomial", 
              data = train)

summary(mod_13)
vif(mod_13)

## Removing JobRole.xLaboratory.Technician
#  p value = 0.046462

mod_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + JobRole.xResearch.Director + 
                MaritalStatus.xSingle + 
                JobInvolvement.xMedium ,family = "binomial", 
              data = train)

summary(mod_14)
vif(mod_14)

## Removing JobInvolvement.xMedium 
#  p value = 0.008348

mod_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + JobRole.xResearch.Director + 
                MaritalStatus.xSingle ,family = "binomial", 
              data = train)

summary(mod_14)
vif(mod_14)

## Removing JobRole.xResearch.Director 
#  p value = 0.007426

mod_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales +MaritalStatus.xSingle ,family = "binomial", 
              data = train)

summary(mod_15)
vif(mod_15)

## Removing BusinessTravel.xTravel_Rarely 
#  p value = 0.004123

mod_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development + 
                Department.xSales +MaritalStatus.xSingle ,family = "binomial", 
              data = train)

summary(mod_16)
vif(mod_16)

## Removing Department.xResearch...Development 
#  p value = 1.50e-05   VIF = 3.823435

mod_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                Department.xSales +MaritalStatus.xSingle ,family = "binomial", 
              data = train)

summary(mod_17)
vif(mod_17)

## Removing Department.xSales 
#  p value = 0.089121 

mod_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently
              + MaritalStatus.xSingle ,family = "binomial", 
              data = train)

summary(mod_18)
vif(mod_18)

## Removing WorkLifeBalance.xBest 
#  p value = 0.000299   VIF = 2.033021

mod_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +
                JobSatisfaction.xVery.High + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently
              + MaritalStatus.xSingle ,family = "binomial", 
              data = train)

summary(mod_19)
vif(mod_19)

## Removing WorkLifeBalance.xGood 
#  p value = 0.041327   

mod_20 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +
                JobSatisfaction.xVery.High + WorkLifeBalance.xBetter + 
                BusinessTravel.xTravel_Frequently
              + MaritalStatus.xSingle ,family = "binomial", 
              data = train)

summary(mod_20)
vif(mod_20)

## Removing TotalWorkingYears 
#  p value = 1.95e-07   VIF = 2.242758 

mod_21 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +
                JobSatisfaction.xVery.High + WorkLifeBalance.xBetter + 
                BusinessTravel.xTravel_Frequently
              + MaritalStatus.xSingle ,family = "binomial", 
              data = train)


summary(mod_21)
vif(mod_21)



##====================================================================================##
# Saving the final model (mod_23) into final_model variable.
# This model is having 10 significant variables.

final_model <- mod_21

##====================================================================================##


##====================================================================================##
##                                                                                    ##
##                           M O D E L   E V A L U A T I O N                          ##
##                                                                                    ##
##====================================================================================##


# Predicting the probabilities of Attrition=1 for test data

test_prediction = predict(final_model, type = "response", newdata =  test[,-2])
# Since Attrition is 2nd column, that's why we have written -2 above.

# Let's see the summary 

summary(test_prediction)

test$Probability <- test_prediction
#View(test)

# Using a probability cut-off of 50%

test_pred_attrition   <- factor(ifelse(test_prediction >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
table(test_pred_attrition,test_actual_attrition)

#                      test_actual_attrition
#   test_pred_attrition   No  Yes
#                    No  1080  166
#                   Yes   23   46

test_confusion_matrix <- confusionMatrix(test_pred_attrition, test_actual_attrition, 
                                         positive = "Yes")
test_confusion_matrix

# Accuracy    : 0.8677  (86% approx)
# Sensitivity : 0.29717 (22% approx)
# Specificity : 0.97733 (98% approx)

# Using a probability cut-off of 40% 

test_pred_attrition   <- factor(ifelse(test_prediction >= 0.40, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
table(test_pred_attrition,test_actual_attrition)

# library e1071 will be used for confusioin matrix creation. 
# This library is preloaded at the initial part of this code.
test_confusion_matrix <- confusionMatrix(test_pred_attrition, 
                                         test_actual_attrition, positive = "Yes")
test_confusion_matrix

# Accuracy    : 0.8555  (85% approx)
# Sensitivity : 0.39151 (33% approx)
# Specificity : 0.94470 (95% approx)

# Finding the optimal probability cut-off value
# Referred from telecom-churn example solution provided.

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_prediction >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values for plotting and storing them in a matrix of 100 X 3 dimensions.

# Generating a sequence
s = seq(.01,.80,length=100)

# Output matrix of 100 x 3 dimensions
OUT = matrix(0,100,3)

for(i in 1:100){
  OUT[i,] = perform_fn(s[i])} 


plot(s, OUT[,1],xlab = "Cutoff",ylab = "Value",cex.lab = 1.5,cex.axis = 1.5,
     ylim = c(0,1),type = "l",lwd = 2,axes = FALSE,col = 2)
axis(1,seq(0,1,length = 5),seq(0,1,length = 5),cex.lab = 1.5)
axis(2,seq(0,1,length = 5),seq(0,1,length = 5),cex.lab = 1.5)
lines(s,OUT[,2],col = "darkgreen",lwd = 2)
lines(s,OUT[,3],col = 4,lwd = 2)
box()
legend(0.68,.65,col = c(2,"darkgreen","darkblue","darkred"),lwd = c(2,2,2,2),
       c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1] - OUT[,2]) < 0.02)]
cutoff
# 0.169596

# Using a probability cut-off of 0.1616162 as calculated above.

test_pred_attrition   <- factor(ifelse(test_prediction >= 0.169596, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
table(test_pred_attrition,test_actual_attrition)

test_confusion_matrix <- confusionMatrix(test_pred_attrition, 
                                         test_actual_attrition, positive = "Yes")
test_confusion_matrix

# Accuracy    : 0.73   ( 73% )
# Sensitivity : 0.7264 (approx 73%)        
# Specificity : 0.7307 (approx 73%)


##====================================================================================##
##                                                                                    ##
##                              K S     S T A T I S I C S                             ##
##                                                                                    ##
##====================================================================================##

test_pred_attrition <- ifelse(test_pred_attrition == "Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition == "Yes",1,0)

pred_object_test <- prediction(test_pred_attrition, test_actual_attrition)
performance_measures_test <- performance(pred_object_test, "tpr", "fpr")

# Lets plot the Area under curve
auc <- performance(pred_object_test,"auc")
unlist(auc@y.values)

# AUC 0.7285747

plot(performance_measures_test,col = "red")
abline(0,1, lty = 8, col = "grey")


ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
(max(ks_table_test))

# KS- STATISTIC 0.4571495

##====================================================================================##
##                                                                                    ##
##                   G A I N      A N D    L I F T      C H A R T                     ##
##                                                                                    ##
##====================================================================================##

lift <- function(labels , predicted_prob,groups=10) {
  if (is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if (is.factor(predicted_prob)) predicted_prob <- 
      as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp = sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain = Cumresp/sum(totalresp)*100,
           Cumlift = Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attr_lift = lift(test_actual_attrition, test_pred_attrition, groups = 10)
Attr_lift
# model is predcting 74% at the 4th decile that means if we sort the employees according to 
# probability , we will be contacting 74% of the employees who are likely to leave the company.(Among 40%)

# At 3rd decile
# Gain - 64.2
# Lift - 2.14

# lets plot gain and lift chart

ggplot(Attr_lift,aes(x = bucket, y = Gain)) + 
  geom_line() + 
  geom_point(col = 'red2', size = 3) +
  geom_text(aes(label = round(Gain,1)),  
            nudge_x = -0.40, nudge_y = -0.4)

ggplot(Attr_lift,aes(x = bucket, y = Cumlift)) + 
  geom_line() + 
  geom_point(col = 'red2', size = 3) +
  geom_text(aes(label = round(Cumlift,2)), 
            nudge_x =-0.1, nudge_y = 0.1)


##====================================================================================##
##                            E N D   O F   S C R I P T                               ##
##====================================================================================##