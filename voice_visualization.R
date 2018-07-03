#set path to cuurent location of file
setwd("C:/Users/Vaibhavi/Documents/Spring 17/BDA/Project")

#reading csv
input_data <- read.csv("voice.csv")

#installing package for visualization
install.packages("ggplot2")
library(ggplot2)

#plot of mean fundamental frequency vs male and female
ggplot(input_data, aes(label, meanfun)) + geom_line(aes(colour = label), size = 4)+ scale_x_discrete("label") + scale_y_continuous("meanfun")+ 
  labs(title = "Mean Fundamental frequency vs Males and females")

#plot of mean frequency vs male and female
ggplot(input_data, aes(label, meanfreq)) + geom_line(aes(colour = label), size = 4)+ scale_x_discrete("label") + scale_y_continuous("meanfreq")+ 
  labs(title = "Mean frequency vs Males and females")

#plot of mean dominant frequency vs male and female
ggplot(input_data, aes(label, meandom)) + geom_line(aes(colour = label), size = 4)+ scale_x_discrete("label") + scale_y_continuous("meandom")+ 
  labs(title = "Mean dominant frequency vs Males and females")

#plot of spectral entropy vs male and female
ggplot(input_data, aes(label, sp.ent)) + geom_line(aes(colour = label), size = 4)+ scale_x_discrete("label") + scale_y_continuous("sp.ent")+ 
  labs(title = "Spectral entropy vs Males and females")

# plot of Mean Dominant Frequency VS Spectral Entropy for Males and Females
ggplot(input_data, aes(meandom, sp.ent, group=label)) + geom_line(aes(colour = label), size = 1) +
  labs(x = "Mean Dominant Frequency", y = "Spectral Entropy", title = "Mean Dominant Frequency VS Spectral Entropy for Males and Females" )

# plot of Mean Dominant Frequency VS Mean Fundamental Frequency for Males and Females
ggplot(input_data, aes(meandom, meanfun, group=label)) + geom_line(aes(colour = label), size = 1) +
  labs(x = "Mean Dominant Frequency", y = "Mean Fundamental Frequency", title = "Mean Dominant Frequency VS Mean Fundamental Frequency for Males and Females") 