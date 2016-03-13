# Set Working Directory
setwd("~/Maryville/EDL735")

# Load Libraries
library(foreign)
library(ggplot2)
library(moments)

# Download, uncompress, and red data
download.file("http://spss5.allenandunwin.com.s3-website-ap-southeast-2.amazonaws.com/Files/sleep5ED.zip",dest="sleep5ED.zip", mode="wb")
unzip("sleep5ED.zip")
sleep5ed = read.spss("sleep5ED/sleep5ED.sav", to.data.frame=TRUE)
attach(sleep5ed)

# Part 1 - basic descriptives
prop.table(table(sex))
summary(age)
prop.table(table(probsleeprec))
summary(hourwnit)
detach(sleep5ed)


# Subset the Data by Sleep Problems
# This seems inelegant
sleep5edclean <- subset(sleep5ed,!(is.na(sleep5ed$totsas)))
noprobs <- subset(sleep5edclean, probsleeprec=="no")
yesprobs <- subset(sleep5edclean, probsleeprec=="yes")
nptotsas <- data.frame(noprobs$totsas)
yptotsas <- data.frame(yesprobs$totsas)

boxplot(totsas ~ probsleeprec, data= sleep5edclean,
        xlab= "Problems with Sleep", ylab = "Sleepiness and Associated Sensations Scale",
        main = "Box and Whisker Plot")

#Skewness and Kurtosis
skewness(noprobs$totsas, na.rm=TRUE)
kurtosis(noprobs$totsas, na.rm=TRUE) -3

#Histogram for NoSleep Problems
ggplot(data.frame(nptotsas), aes(x = noprobs$totsas)) + 
  geom_histogram(aes(y = ..density..), breaks=seq(5, 50, by = 2), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Sleepiness and Associated Sensations Scale'))) + 
  ylab(expression(bold('Density'))) +
  ggtitle("Reports No Sleep Problems")

#Skewness and Kurtosis
skewness(yesprobs$totsas, na.rm=TRUE)
kurtosis(yesprobs$totsas, na.rm=TRUE) -3

#Histogram for Yes Sleep Problems
ggplot(data.frame(yptotsas), aes(x = yesprobs$totsas)) + 
  geom_histogram(aes(y = ..density..), breaks=seq(5, 50, by = 2), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + 
  xlab(expression(bold('Sleepiness and Associated Sensations Scale'))) + 
  ylab(expression(bold('Density'))) + 
  ggtitle("Reports Sleep Problems")

#Generate Q-Q plots
qqnorm(noprobs$totsas, ylab = "No Sleep Problems") ; qqline(noprobs$totsas)
qqnorm(yesprobs$totsas, ylab = "Yes Sleep Problems") ; qqline(yesprobs$totsas)