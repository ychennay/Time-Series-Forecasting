# for Professor Yu's Business Forecasting final project. Code is my own, data is from publically available Zillow dataset

forecast.df <- read.csv(file.choose())
summary(forecast.df)
str(forecast.df)

#turn coastal into a "factor"
forecast.df$Coastal
forecast.df$Coastal<- factor(forecast.df$Coastal, labels = c("No", "Yes"))

#turn ML Sports column into a numeric column
forecast.df$ML.Sports <-as.numeric(forecast.df$ML.Sports)

#turn the remaining columns into vectors from a percent
#for(i in c(3, 7:76))
#{forecast.df[,i]<- as.character(forecast.df[,i])
#forecast.df[,i] = substr(forecast.df[,i],1,nchar(forecast.df[,i])-1)
#forecast.df[,i]<- as.numeric(forecast.df[,i])/100
#forecast.df[,i]<- as.vector(forecast.df[,i])
#}

#turn the remaining columns into numbers vectors from "factors"
for(i in c(3, 7:76))
{
  forecast.df[,i]<- as.numeric((forecast.df[,i]))
  forecast.df[,i]<- as.vector(forecast.df[,i])
}
warnings()

str(forecast.df)
#make PricetoRent10 Null, since it has all NAs
forecast.df$pricetorent10<- NULL

#survey the data to find any big trends
pairs(forecast.df[5:20])

#analyze the correlation between the variables
cor(forecast.df[,c(3,6:20)], use="pairwise.complete.obs")

sapply(forecast.df, is.numeric)
cor(forecast.df[,6:20], use="pairwise.complete.obs")

#get rid of columns with no data 
forecast.df$pricetorent2005<- NULL
forecast.df$perdifmedlist2005<- NULL
forecast.df$medlistprice2005<- NULL
str(forecast.df)

install.packages("corrplot")
library(corrplot)
library(gplots)
par(cex=0.8)
corrplot.mixed(corr=cor(forecast.df[,c(7:10,11:25)], use="pairwise.complete.obs"))
corrplot.mixed(corr=cor(forecast.df[,c(7:10,26:40)], use="pairwise.complete.obs"))
corrplot.mixed(corr=cor(forecast.df[,c(7:10,41:55)], use="pairwise.complete.obs"))
corrplot.mixed(corr=cor(forecast.df[,c(7:10,56:72)], use="pairwise.complete.obs"))


par(cex=2)

corrplot.mixed(corr=cor(forecast.df[,7:10], use="pairwise.complete.obs"))



#find any relationship between coastal cities and median sale price per sq. foot
coastalanalysis5y <- aggregate( mspsf5 ~ Coastal, data=forecast.df, mean)
coastalanalysis5y
                  
coastalanalysis1y <- aggregate( mspsf1 ~ Coastal, data=forecast.df, mean)
coastalanalysis1y

coastalanalysis2y <- aggregate( mspsf2 ~ Coastal, data=forecast.df, mean)
coastalanalysis2y

coastalanalysis10y <- aggregate( mspsf10 ~ Coastal, data=forecast.df, mean)
coastalanalysis10y

#there is no statistical significant relationship btw coast and mspsf
t.test(mspsf10 ~ Coastal, data=forecast.df)

#plotting density of mspsf columns
mspsf10plot <- ggplot(forecast.df, aes(mspsf10))+ geom_density(kernel= "gaussian", color="blue", fill="blue")+ xlab("10 Year Change in Median Sale Price / Sq. Foot")+ylab("Density")+ggtitle("Density Plot of 10Y Median Sale Price Growth / Sq. Foot")+geom_vline(xintercept = 0)
mspsf5plot <- ggplot(forecast.df, aes(mspsf5))+ geom_density(kernel= "gaussian", color="pink", fill="pink")+ xlab("5 Year Change in Median Sale Price / Sq. Foot")+ylab("Density")+ggtitle("Density Plot of 5Y Median Sale Price Growth / Sq. Foot")+geom_vline(xintercept = 0)
mspsf2plot <- ggplot(forecast.df, aes(mspsf2))+ geom_density(kernel= "gaussian", color="lightgreen", fill="lightgreen")+ xlab("2 Year Change in Median Sale Price / Sq. Foot")+ylab("Density")+ggtitle("Density Plot of 2Y Median Sale Price Growth / Sq. Foot")+geom_vline(xintercept = 0)
mspsf1plot <- ggplot(forecast.df, aes(mspsf1)) + geom_density(kernel= "gaussian", color="lightblue", fill="lightblue")+ xlab("1 Year Change in Median Sale Price / Sq. Foot")+ylab("Density")+ggtitle("Density Plot of 1Y Median Sale Price Growth / Sq. Foot")+geom_vline(xintercept = 0)
grid.arrange(mspsf10plot, mspsf5plot, mspsf2plot, mspsf1plot, ncol=2)

#plotting relationship of MSPSF and Price to Rent
par(cex=1)
#1Y Price to Rent vs. MSPSF
ptrmspsf1 <- ggplot(forecast.df, aes(pricetorent1, mspsf1, color=Coastal, size=pop2015))+geom_point()+geom_text(aes(label=ifelse(pop2015>5000000, as.character(Zillow.Region.name),'')),hjust=1,vjust=2)+ylim(-0.025,0.125)+xlim(-0.04,0.10)+geom_smooth(method=lm)+xlab("1 Year Price to Rent Change")+ylab("1 Year Median Sale Price / Square Foot Change")+ggtitle("1Y Growth in Price to Rent vs. Median Sale Price / Square Foot")+theme(legend.position="none")+geom_vline(xintercept = 0)+geom_hline(yintercept=0)

#2Y Price to Rent vs. MSPSF
ptrmspsf2 <- ggplot(forecast.df, aes(pricetorent2, mspsf2, color=Coastal, size=pop2015))+geom_point()+geom_text(aes(label=ifelse(pop2015>5000000, as.character(Zillow.Region.name),'')),hjust=1,vjust=2)+ylim(0,0.3)+xlim(-0.1,0.20)+geom_smooth(method=lm)+xlab("2 Year Price to Rent Change")+ylab("2 Year Median Sale Price / Square Foot Change")+ggtitle("2Y Growth in Price to Rent vs. Median Sale Price / Square Foot")+theme(legend.position="none")+geom_vline(xintercept = 0)+geom_hline(yintercept=0)
 
#5Y Price to Rent vs. MSPSF
ptrmspsf5 <- ggplot(forecast.df, aes(pricetorent5, mspsf5, color=Coastal, size=pop2015))+geom_point()+geom_text(aes(label=ifelse(pop2015>5000000, as.character(Zillow.Region.name),'')),hjust=1,vjust=2)+ylim(-0.13,0.5)+xlim(-0.3,0.50)+geom_smooth(method=lm)+xlab("5 Year Price to Rent Change")+ylab("5 Year Median Sale Price / Square Foot Change")+ggtitle("5Y Growth in Price to Rent vs. Median Sale Price / Square Foot")+theme(legend.position="none")+geom_vline(xintercept = 0)+geom_hline(yintercept=0)
 
grid.arrange(ptrmspsf5,ptrmspsf2,ptrmspsf1, ncol=3)

ptrmspsf1lm <- lm(mspsf1 ~ pricetorent1, data=forecast.df)
ptrmspsf1lm$residuals
ptrmspsf1lm$fitted.values
plot(ptrmspsf1lm$fitted.values,ptrmspsf1lm$residuals)
summary(ptrmspsf1lm)


par(mfrow=c(1,1))
foreclosuremspsft5<-lm(mspsf5 ~ foreclosure2010, data=forecast.df)
summary(foreclosuremspsft5)


#relationship between foreclosure rate and MSPSF 5 years
foreclosemspsf5 <- ggplot(forecast.df, aes(foreclosure2010, mspsf5, size=pop2015))+geom_point()+geom_text(aes(label=ifelse(pop2015>3000000, as.character(Zillow.Region.name),'')),hjust=-0.1,vjust=2)+ylim(-0.1,0.35)+xlim(0,25)+geom_smooth(method=lm)+xlab("2010 Foreclosure Rate (out of 10,000 Homes)")+ylab("5 Year Median Sale Price / Square Foot Change")+ggtitle("Relationship Between Foreclosure Rate and Median Sales Price / Square Foot Growth")+theme(legend.position="none")+geom_vline(xintercept = 0)+geom_hline(yintercept=0)
foreclosemspsf5
#significant relationship at 5Y Level
summary(lm(mspsf5 ~ foreclosure2010, data=forecast.df))


#relationship between foreclosure rate and MSPSF 10 years
foreclosemspsf10 <- ggplot(forecast.df, aes(foreclosure2005, mspsf10, size=pop2015))+geom_point()+geom_text(aes(label=ifelse(pop2015>3000000, as.character(Zillow.Region.name),'')),hjust=-0.1,vjust=2)+ylim(-0.1,0.3)+xlim(0,12)+geom_smooth(method=lm)+xlab("2005 Foreclosure Rate (out of 10,000 Homes)")+ylab("10 Year Median Sale Price / Square Foot Change")+ggtitle("Relationship Between Foreclosure Rate and Median Sales Price / Square Foot Growth")+theme(legend.position="none")+geom_vline(xintercept = 0)+geom_hline(yintercept=0)
foreclosemspsf10
#significant relationship at 10Y Level
summary(lm(mspsf10 ~ foreclosure2005, data=forecast.df))


#relationship between foreclosure rate and MSPSF 2 years
foreclosemspsf2 <- ggplot(forecast.df, aes(foreclosure2014, mspsf2, size=pop2015))+geom_point()+geom_text(aes(label=ifelse(pop2015>3000000, as.character(Zillow.Region.name),'')),hjust=-0.1,vjust=2)+ylim(0,0.25)+xlim(0,12)+xlab("20014 Foreclosure Rate (out of 10,000 Homes)")+ylab("2 Year Median Sale Price / Square Foot Change")+ggtitle("Relationship Between Foreclosure Rate and Median Sales Price / Square Foot Growth")+theme(legend.position="none")+geom_vline(xintercept = 0)+geom_hline(yintercept=0)
foreclosemspsf2
#insignificant relationship at 2Y Level
summary(lm(mspsf2 ~ foreclosure2014, data=forecast.df))
grid.arrange(foreclosemspsf2, foreclosemspsf5, foreclosemspsf10, ncol=3)


#STATISTICALLY SIGNIFICANT RELATIONSHIPS
summary(lm(mspsf10 ~ humancaplevel, data=forecast.df))
summary(lm(mspsf10 ~ totemp10, data=forecast.df))
summary(lm(mspsf10 ~ totemp10, data=forecast.df))
summary(lm(mspsf10 ~ foreclosure2010, data=forecast.df))

foreclose2010mspsf10 <- ggplot(forecast.df, aes(foreclosure2010, mspsf10, size=pop2015))+geom_point()+geom_text(aes(label=ifelse(pop2015>3000000, as.character(Zillow.Region.name),'')),hjust=-0.1,vjust=2)+ylim(-0.1,0.3)+xlim(0,12)+geom_smooth(method=lm)#+xlab("2005 Foreclosure Rate (out of 10,000 Homes)")+ylab("10 Year Median Sale Price / Square Foot Change")+ggtitle("Relationship Between Foreclosure Rate and Median Sales Price / Square Foot Growth")+theme(legend.position="none")+geom_vline(xintercept = 0)+geom_hline(yintercept=0)
foreclose2010mspsf10

#not significant
summary(lm(mspsf10 ~ medsalesprice2005, data=forecast.df))
summary(lm(mspsf10 ~ perdifmedlist2010, data=forecast.df))

#2 YEAR MSPPSF vs. Median List Price in 2013
summary(lm(mspsf2 ~ medlistprice2013, data=forecast.df))
t<--0.00000042
50000*t
medlistprice2y <- ggplot(forecast.df, aes(medlistprice2013, mspsf2, size=pop2015))+geom_point()+geom_text(aes(label=ifelse(pop2015>3000000, as.character(Zillow.Region.name),'')),hjust=-0.1,vjust=2)#+ylim(-0.1,0.3)+xlim(0,12)+geom_smooth(method=lm)#+xlab("2005 Foreclosure Rate (out of 10,000 Homes)")+ylab("10 Year Median Sale Price / Square Foot Change")+ggtitle("Relationship Between Foreclosure Rate and Median Sales Price / Square Foot Growth")+theme(legend.position="none")+geom_vline(xintercept = 0)+geom_hline(yintercept=0)
medlistprice2y




grid.arrange(foreclosemspsf10, foreclosemspsf5, ncol=2)
plot(forecast.df$foreclosure2010, forecast.df$mspsft1)

