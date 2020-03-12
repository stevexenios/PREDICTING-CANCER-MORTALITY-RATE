####################################################################################################
# STEVE G MWANGI & TENKI KO
# EXPLORATORY DATA ANALYSIS
# 03/10/2020
##################################################################################################
setwd(choose.dir())
eda1.df = read.csv(file.choose())
###################################################################################################
# Verifying correct data
# Viewing the first 6 lines of the data frame
head(eda1.df) # Checking the first 6 lines of the data frame
dim(eda1.df) 
summary(eda1.df)
###################################################################################################
str(eda1.df)

# Create two Data Frames for QUalitative and Quantitative variables:
quant.df <-data.frame(eda1.df$MedHouInc, eda1.df$AgeDiagnosed, eda1.df$MortalityRate)
summary(quant.df)
library("dlookr")
###################################################################################################
# After installing the dlookr package, we are going to use it to understand the distribution of 
# data by calculating descriptive statistics of numerical data. In addition, correlation between
# variables is identified and normality test is performed. It also identifies the relationship 
# between target variables and independent variables.

# describe() Computes the descriptive statistics for numerical data.
# The desriptive statistics help determine the distribution of numerical variables.
# Like the function of dplyr, the first argument is the dataframe. The second and
# subsequent arguments refer to variables within that data frame.
describe(quant.df, cols = c(eda1.df.MedHouInc, eda1.df.AgeDiagnosed, eda1.df.MortalityRate))

# Sorting the data by left or right skewness size
quant.df %>%
  describe() %>%
  select(variable, skewness, mean, p25, p50, p75) %>%
  filter(!is.na(skewness)) %>%
  arrange(desc(abs(skewness)))

# Grouping by Gender and Cancer Type
eda1.df %>%
  group_by(CANCER) %>%
  describe(MedHouInc, AgeDiagnosed, MortalityRate)

# Testing of Normality using the function normality()
# Shapiro-Wilk normality test
normality(quant.df)
normality(eda1.df)

# Setting the variable insitu or malignant column as a factor
eda1.df$INSvsMALI = factor(eda1.df$INSvsMALI)
# Creating a paired scatter plot for each quantitative variable
plot(eda1.df)
#eda_report(eda1.df, output_format = "html")
