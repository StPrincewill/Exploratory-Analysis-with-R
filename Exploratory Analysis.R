##################################################
###                 ## 
##################################################
#                                               ##
##################################################
# Written by Princewill Iheanacho
#
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
setwd("C:/Users/Dell/Desktop/Data Analysis")

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

#if(!require(dplyr)){install.packages("dplyr")}
#library("dplyr")

if(!require(lattice)){install.packages("lattice")}
library("lattice")

##################################################
### Read in Data                                ##
##################################################

AS01_PI <- read.csv("AS01.csv", header = TRUE, sep = ",")
str(AS01_PI)

## Summarizing that for the NDP party
## The total number of votes earned by NDP in each of the provinces

Table1_PI <- aggregate(x = AS01_PI$NDV, by = list(AS01_PI$Prov), FUN=sum, na.rm = TRUE)
Table1_PI

## The weighted mean score for Mulcair in Manitoba

weighted.mean(AS01_PI[AS01_PI$Prov == "MB","Mul"], w = AS01_PI[AS01_PI$Prov == "MB","Electors"], na.rm = TRUE)

## Satndard devaition for economy satisfaction

Table2_PI <- aggregate(x = AS01_PI$ecc_sat, by = list(AS01_PI$Win_Party), FUN=sd, na.rm = TRUE)
Table2_PI

## OR

sd(AS01_PI[AS01_PI$Win_Party =="NDP","ecc_sat"], na.rm = TRUE)

## The 68th percentile of females answering LPP2015

quantile(AS01_PI$Female, c(.68), na.rm = TRUE)

## The mean absolute deviation for Turnout for electoral district in Ontario

mad(AS01_PI[AS01_PI$Prov == "ON","TO"], na.rm = TRUE)


# Let's create a summary table

Table3_PI <- table(AS01_PI$Prov, AS01_PI$Win_Party)
Table3_PI

prop.table(Table3_PI, 1) # row percentages

# Creating Bar Charts

# Remove Nunavut, Northwest Territories and Yukon from the analysis

AS01_2_PI <- AS01_PI[!(AS01_PI$X%in% c("336", "337","338")), ]
AS01_2_PI

barchart(Win_Party ~ Male + Female,
         data=AS01_2_PI, beside=TRUE, main="Gender By Winning Party ",
         xlab="Gender", ylab="Winning Party", 
         auto.key=list(space='bottom'))

# Creating Histogram
# Voters feeling about Mulcair

histogram( ~ Mul, dat=AS01_2_PI, type = "count",
           main="Mulcair Count of Support")

# Creating Boxplots

bwplot(NDP ~ Win_Party, data=AS01_2_PI, 
       main="Distribution of NDP Support by Winning Party",
       xlab="Winning Party",  pch = '|')

# Creating scatter plots

# First create a histogram for voters feeling about the NDP
histogram( ~ NDP, data=AS01_2_PI, type = "count",
           main="NDP Count of Support")

# Histogram about Mulcair
histogram( ~ Mul, dat=AS01_2_PI,
           main="Mulcair Percentage of Support")

# Scatterplot showing relationship betweeen NDP and Mulcair

xyplot(NDP ~ Mul, data=AS01_PI, color="violet", pch=20,
       main="NDP In Relation to Mulcair")



