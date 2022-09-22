

#Substitute NA values with 0

sales[is.na(sales)] = 0
digital[is.na(digital)] = 0
investments[is.na(investments)] = 0


#By Month

library(lubridate)
library(dplyr)

#round dates down to week
sales$month <- floor_date(sales$SDESC, "month")
investments$month <- floor_date(investments$Date, "month")
digital$month <- floor_date(digital$week.ending.Sabato, "month")


sales=aggregate(sales[,4:369], by=list(sales$month), FUN=sum)
investments=aggregate(investments[,2:14], by=list(investments$month), FUN=sum)
digital=aggregate(digital[,4:15], by=list(digital$month), FUN=sum)




# D1.5
#Firstly, i'm going to open all the variable i need to conduct the analysis

library(fpp2)
#I'm opening the file which contains
#(from each dataset i'm taking out the sub-dataset that i need; one with the period related column,
#to create the time series, and one without)

#focus group sales in submarket 1
sales_focusgroup <- sales[, c(1,80,81)] 
sale_focusgroup <- sales[, c(80,81)] #taking out the time series variable
#time series creation
volumesales_focusgroup<- ts(sales_focusgroup$Volume.Sales..MSU....82, start=c(2017,7), end=c(2021,7), frequency = 12) 
valuesales_focusgroup<- ts(sales_focusgroup$Value.Sales..MLC....83, start=c(2017,7), end=c(2021,7), frequency = 12) 


#focus group digital clicks received
digital_clicks=digital[, c(1,2,3)]
digital_clicks[is.na(digital_clicks)] = 0
digital_clicks<- digital_clicks[-c(1),]
digital_click=digital[, c(2,3)] #taking out the time series variable
digital_click[is.na(digital_click)] = 0
digital_click<- digital_click[-c(1),]


#focus group digital impressions received
digital_impressions=digital[, c(1,4,5,6,7,8,9,10,11,12,13)]
digital_impressions[is.na(digital_impressions)] = 0
digital_impressions<- digital_impressions[-c(1),]
digital_impression=digital[, c(4,5,6,7,8,9,10,11,12,13)] #taking out the time series variable
digital_impression[is.na(digital_impression)] = 0
digital_impression<- digital_impression[-c(1),]


#digital total
digital_tots=digital[, c(1,2,3,4,5,6,7,8,9,10,11,12,13)]
digital_tots<- digital_tots[-c(1),]
digital_tot=digital[, c(2,3,4,5,6,7,8,9,10,11,12,13)] #taking out the time series variable
digital_tot<- digital_tot[-c(1),]


#focus group investments in digital clicks
investments_clicks=investments[, c(1,2,10)]
investments_clicks[is.na(investments_clicks)] = 0
investments_clicks<- investments_clicks[-c(1),]
investments_click=investments[, c(2,10)]#taking out the time series variable
investments_click[is.na(investments_click)] = 0
investments_click<- investments_click[-c(1),]


#focus group investments in digital impressions
investments_impressions=investments[, c(1,2,3,4,5,6,7,8,9,11,12)]
investments_impressions[is.na(investments_impressions)] = 0
investments_impressions<- investments_impressions[-c(1),]
investments_impression=investments[, c(2,3,4,5,6,7,8,9,11,12)] #taking out the time series variable
investments_impression[is.na(investments_impression)] = 0
investments_impression<- investments_impression[-c(1),]


#investments totals
investments_tots=investments[, c(1,2,3,4,5,6,7,8,9,10,11,12)]
investments_tots<- investments_tots[-c(1),]
investments_tot=investments[, c(2,3,4,5,6,7,8,9,10,11,12)] #taking out the time series variable
investments_tot<- investments_tot[-c(1),]




#making of correlation plot
install.packages("corrplot")
library(corrplot)

#corrplot investments
SalesandDigital_investments=data.frame(volumesales_focusgroup,investments_click,investments_impression)
x11()
corrplot(cor(SalesandDigital_investments),        # Correlation matrix
         method = "number", # Correlation plot method
         type = "full",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         bg = "white",     # Background color
         title = "",       # Main title
         tl.pos = "l",
         col = NULL)   




#PCA
#PCA application on impressions investments
library(MASS)
library(car)
library(rgl)

#standardization
investments_tot.sd=scale(investments_tot)
investments_tot.sd=data.frame(investments_tot.sd)

#pca matrix creation
investmentstot.pca=data.frame(investments_tot.sd)

#principal components on numerical variables
pc.investmentstot<- princomp(investments_tot.sd, scores=T)
summary(pc.investmentstot)

#visualize the relevant components
x11()
#layout(matrix(c(2,3,1,3),2,byrow=T))
# variance of PC
barplot(pc.investmentstot$sdev^2, las=2, main='Principal Components', ylab='Variances')

# original variances 
x11()
barplot(sapply(investments_tot.sd,sd)^2, las=2, main='Original Variables', ylab='Variances')

# proportion of explained variance
x11()
plot(cumsum(pc.investmentstot$sdev^2)/sum(pc.investmentstot$sde^2), type='b', axes=F, xlab='Number of components', ylab='Contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(investments_tot.sd),labels=1:ncol(investments_tot.sd),las=1)



#show the results of the first x components
# loadings (coefficients of the linear combination of the variables that define each component)
load.investmentstot<- pc.investmentstot$loadings

# loadings of the first 3 PCs
x11()
par(mar = c(1,4,0,2), mfrow = c(5,1))
for(i in 1:5) barplot(load.investmentstot[,i], ylim = c(-1, 1))


