####################################################################################################
# STEVE G MWANGI & TENKI KO
# EXPLORATORY DATA ANALYSIS
# MILESTONE 4
# 02/21/2020
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
#1

# Creating a 1 By 3 layout for 3 Scatterplots plots
par(mfrow=c(1,3), mar=c(3.5,3.5,3.5,0.5),mgp=c(2.25,0.5,0),las=1) 
# Scatterplots Insitu_Malignant
plot(eda1.df$INSvsMALI, 
     eda1.df$MortalityRate, 
     xlab="INSITU_MALIGNANT",
     ylab="MORTALITY_RATE",
     col = "red", 
     pch=16, 
     main = "INSITU_MALIGNANT VS MORTALITY_RATE")
a1.lm = lm(MortalityRate~INSvsMALI, data = eda1.df) # Creating a linear model
# Adding fitted line
abline(a1.lm$coefficients, lwd=1)

# Scatterplots MedHouIncome
plot(eda1.df$MedHouInc, 
     eda1.df$MortalityRate, 
     xlab="MedHouInc",
     ylab="MORTALITY_RATE",
     col = "green", 
     pch=16, 
     main = "Median Household Income VS Mortality Rate")
a2.lm = lm(MortalityRate~MedHouInc, data = eda1.df) # Creating a linear model
# Adding fitted line
abline(a2.lm$coefficients, lwd=1)

# Scatterplots AgeDiagnosed
plot(eda1.df$AgeDiagnosed, 
     eda1.df$MortalityRate, 
     xlab="AgeDiagnosed",
     ylab="MORTALITY_RATE",
     col = "orange", 
     pch=16, 
     main = "AgeDiagnosed VS Mortality Rate")
a3.lm = lm(MortalityRate~AgeDiagnosed, data = eda1.df) # Creating a linear model
# Adding fitted line
abline(a3.lm$coefficients, lwd=1)


#2

# Creating a 1 By 3 layout for 3 Scatterplots plots
par(mfrow=c(1,3), mar=c(3.5,3.5,3.5,0.5),mgp=c(2.25,0.5,0),las=1) 

# Scatterplots MedHouInc vs INSITU_MALIGNANT
plot(eda1.df$INSvsMALI, 
     eda1.df$MedHouInc, 
     xlab="INSITU_MALIGNANT",
     ylab="MedHouInc",
     col = "red", 
     pch=16, 
     main = "MedHouInc vs INSITU_MALIGNANT")
a4.lm = lm(MedHouInc~INSvsMALI, data = eda1.df) # Creating a linear model
# Adding fitted line
abline(a4.lm$coefficients, lwd=1)

# Scatterplots AgeDiagnosed VS MedHouInc
plot(eda1.df$MedHouInc, 
     eda1.df$AgeDiagnosed, 
     xlab="MedHouInc",
     ylab="AgeDiagnosed",
     col = "green", 
     pch=16, 
     main = "AgeDiagnosed VS MedHouInc")
a5.lm = lm(AgeDiagnosed~MedHouInc, data = eda1.df) # Creating a linear model
# Adding fitted line
abline(a5.lm$coefficients, lwd=1)

# Scatterplots INSvsMALI vs AgeDiagnosed
plot(eda1.df$AgeDiagnosed, 
     eda1.df$INSvsMALI, 
     xlab="AgeDiagnosed",
     ylab="INSvsMALI",
     col = "orange", 
     pch=16, 
     main = "INSvsMALI vs AgeDiagnosed")
a6.lm = lm(INSvsMALI~AgeDiagnosed, data = eda1.df) # Creating a linear model
# Adding fitted line
abline(a6.lm$coefficients, lwd=1)

#3
# Creating a 1 By 2 layout for 2 Boxplots plots
par(mfrow=c(1,2), mar=c(3.5,3.5,3.5,0.5),mgp=c(2.25,0.5,0),las=1) 
# Boxplots for 4 Levels of Cancer
plot(eda1.df$CANCER, 
     eda1.df$MortalityRate, 
     xlab="CANCER TYPE",
     ylab="MORTALITY RATE", 
     pch=16, 
     main = "MORTALITY RATE Vs. CANCER TYPE", 
     col = c("red", "green", "orange", "black"))

# Boxplots for 2 Levels of Gender
plot(eda1.df$GENDER, 
     eda1.df$MortalityRate, 
     xlab="GENDER",
     ylab="MORTALITY RATE", 
     pch=16, 
     main = "MORTALITY RATE Vs. GENDER", 
     col = c("pink", "blue"))

###################################################################################################
# Numerical Summaries,..
summary(eda1.df$GENDER)
summary(eda1.df$CANCER)
summary(eda1.df$INSvsMALI)
summary(eda1.df$MedHouInc)
summary(eda1.df$AgeDiagnosed)
summary(eda1.df$MortalityRate)

eda1.lm = lm(MortalityRate~INSvsMALI+CANCER+MedHouInc+INSvsMALI+AgeDiagnosed, data = eda1.df) # Creating a linear model
summary(eda1.lm)
