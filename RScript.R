

r = getOption("repos")
r["CRAN"]="http://cran.us.r-project.org"
options(repos=r)


#1. Print "Plotting Basics: Lastname"
print("Plotting Basics: Mohile")     


#2.installing required packages
# install.packages('FSA')
# install.packages('FSAdata')
# install.packages('magrittr')
# install.packages('dplyr')
# install.packages('plotrix')
# install.packages('ggplot2')
# install.packages('moments')
#.importing required libraries
install.packages('FSA')              
install.packages('FSAdata')
install.packages('magrittr')
install.packages('dplyr')
install.packages('plotrix')
install.packages('ggplot2')
install.packages('moments')



library('FSA')
library('FSAdata')
library('magrittr')
library('dplyr')
library('plotrix')
library('ggplot2')
library('moments')


#3.loading the data
BullTroutRML2                      


#4.Printing the first and last 3 records from the BullTroutRMS2 dataset
head(BullTroutRML2,n=3)           
tail(BullTroutRML2,n=3) 


#5. Remove all records except those from Harrison Lake
harrison_data_new <- filter(BullTroutRML2, lake=='Harrison')      
harrison_data_new


#6. Display the first and last 5 records from the filtered BullTroutRML2 dataset
head(harrison_data_new, n=5)            
tail(harrison_data_new,n=5)


#7. Display the structure of the filtered BullTroutRML2dataset
str(harrison_data_new)               


#8. Display the summary of the filtered BullTroutRML2dataset
summary(harrison_data_new)           


#9. Create a scatterplot for "age" (y variable) and "fl" (x variable) with the following specifications:
# Limit of x axis is (0,500)
# Limit of y axis is (0,15)
# Title of graph is "Plot 1: Harrison Lake Trout
# Y axis label is "Age (yrs)"
# X axis label is "Fork Length (mm)"
# Use a small filled circle for the plotted data points
install.packages('ggplot2')         
library('ggplot2')

?ggplot
scatter_plot <- ggplot(harrison_data_new, aes(x=fl,y=age)) +
                geom_point(colour ="blue",size = 3) + 
                xlim(0,500) + ylim(0,15) + labs(x="Fork Length(mm)",
                y="Age(yrs)",title="Plot 1: Harrison Lake Trout")+
                theme(plot.title = element_text(hjust = 0.5))
scatter_plot           


#10. Plot an "Age" histogram with the following specifications
#Y axis label is "Frequency"
#X axis label is "Age (yrs)"
#Title of the histogram is "Plot 2: Harrison Fish Age Distribution"
#X and Y axis limits is 0, 15
#The color of the frequency plots is "cadetblue"
#The color of the Title is "cadetblue"
#to plot the scatter plot graph
hist(harrison_data_new$age,xlab = "Age(yrs)", ylab = "Frequency", 
          main = "Plot 2:Harrison Fish Age Distribution", xlim = c(0,15),
          ylim = c(0,15),col = 'cadetblue',col.axis='cadetblue',col.main = 'cadetblue')

fl <- harrison_data_new$fl
age <- harrison_data_new$age
#11. Create an overdense plot using the same specifications as the previous scatterplot. But,
#Title the plot "Plot 3: Harrison Density Shaded by Era"
#Y axis label is "Age (yrs)"
#Y axis limits are 0 to 15
#X axis label is "Fork Length (mm)"
#X axis limits are 0 to 500
#include two levels of shading for the "green" data points.
#Plot solid circles as data points

overdense_plot <- plot(age ~ fl,
                       main = "Plot 3:Harrison Density Shaded by Era",
                       xlab = "Fork Length(mm)",
                       ylab = "Age(yrs)",
                       xlim = c(0,500),
                       ylim = c(0,15),
                       pch = 16,
                       col = rgb(0,(1:2)/2,0))


#12. Create a new object called "tmp" that includes the first 3 and last 3 records of the BullTroutRML2 data set.
tmp <- headtail(BullTroutRML2,n=3)
tmp


#13. Display the "era" column (variable) in the new "tmp" object
tmp$era


#14.Create a pchs vector with the argument values for + and x.
pchs <- c('+','x')
pchs


#15. Create a cols vector with the two elements "red" and "gray60"
cols <- c("red","gray60")
cols


#16.Convert the tmp era values to numeric values.
conversion <- as.numeric(tmp$era)
conversion


#17. Initialize the cols vector with the tmp era values\
tmp$era <- cols
tmp$era


#18. Create a plot of "Age (yrs)" (y variable) versus "Fork Length (mm)" (x variable) with the following specifications:
#Title of graph is "Plot 4: Symbol & Color by Era"
#Limit of x axis is (0,500)
#Limit of y axis is (0,15)
#X axis label is "Age (yrs)"
#Y axis label is "Fork Length (mm)"
#Set pch equal to pchs era values
#Set col equal to cols era values
plot(age~fl, data= harrison_data_new, main = "Plot 4: Symbol & Color by Era", 
     ylab = "Age (yrs)", ylim = c(0,15), xlab ="Fork Length (mm)",xlim = c(0,500), 
     pch=pchs[harrison_data_new$era] , col=cols[harrison_data_new$era])


#19.Plot a regression line overlay on Plot 4 and title the new graph "Plot 5: Regression Overlay".
plot(age~fl, data= harrison_data_new, main = "Plot 5: Regression Overlay", 
     ylab = "Age (yrs)", ylim = c(0,15), xlab ="Fork Length (mm)",xlim = c(0,500), 
     pch=pchs[harrison_data_new$era] , col=cols[harrison_data_new$era])
rl <- lm(age~fl,data = harrison_data_new)
abline(rl)


#20. Place a legend of on Plot 5 and call the new graph "Plot 6: :Legend Overlay"
plot(age~fl, data= harrison_data_new, main = "Plot 6: Legend Overlay", 
     ylab = "Age (yrs)", ylim = c(0,15), xlab ="Fork Length (mm)",xlim = c(0,500), 
     pch=pchs[harrison_data_new$era] , col=cols[harrison_data_new$era])
rl <- lm(age~fl,data = harrison_data_new)
abline(rl)
?legend
harrison_data_new$era #to find the levels.
legend("topleft",legend = c("1977-80","1997-01"), pch = pchs, col = cols)





