#########################
## Amanda Chen ##########
## Final Project ########
## The Effect of the 2012 Lawsuit on Diversity in "The Bachelor" and "The Bachelorette"
#########################

setwd("~/R")
bachelor<- read.csv("bachelordata.csv")

#creating a subset of data so information about leads only appear once
leads <- unique(bachelor$LeadFullName)
leadsdata <-  subset(bachelor, LeadFullName == leads[1])
leadsdata <- leadsdata[1, ]
for(i in 2:length(leads)){
  subdata <-  subset(bachelor, LeadFullName == leads[i])
  leadsdata <- rbind(leadsdata, subdata[1, ])
}

#proportion of minority leads before 2012 vs. after 2012
## creating assigning LeadRace to numeric values, taking subset of data
leadsdata$minoritylead[leadsdata$LeadRace== "Black"]<- 1 
leadsdata$minoritylead[leadsdata$LeadRace== "Latino"]<- 1 
leadsdata$minoritylead[leadsdata$LeadRace== "White"]<- 0
table(leadsdata$minoritylead)
minoritylead<- leadsdata$minoritylead

pre2012lead<- subset(leadsdata, (year<= 2012))
post2012lead<- subset(leadsdata, (year> 2012))
leadouts<-c(mean(pre2012lead$minoritylead), mean(post2012lead$minoritylead))

##graphing results
pdf("FinalProjectLeadBeforeAfter.pdf", width=6, height=5)
barplot(leadouts,
        names= c("Minority Leads Pre-2012", "Minority Leads Post-2012"),
        col= "blue",
        main= "Mean Lead Minorities Before 2012 vs. After 2012",
        ylab= "Mean Lead Minorities",
        ylim= c(0,0.20))
dev.off()

##using prop.test to test hypothesis
sum(pre2012lead$minoritylead)
sum(post2012lead$minoritylead)
testleads<- prop.test(x=c(0, 2), n=c(24, 12))
testleads

#proportion of minority contestants before 2012 vs. after 2012
pre2012minority<- subset(bachelor, (year<= 2012))
post2012minority<- subset(bachelor, (year> 2012))
outs<- c(mean(pre2012minority$minority, na.rm=T), mean(post2012minority$minority, na.rm=T))

##graphing results
pdf("FinalProjectMinorityBeforeAfter.pdf", width=6, height=5)
barplot(outs,
        names= c("Minority Contestants \n Pre-2012", "Minority Contestants \n Post-2012"),
        col= "blue",
        main= "Mean Minority Contestants Before 2012 vs. After 2012",
        ylab= "Mean Minority Contestants",
        ylim= c(0,0.25))
dev.off()

##using prop.test to test hypothesis
sum(pre2012minority$minority, na.rm=T)
sum(post2012minority$minority, na.rm=T)
test<- prop.test(x=c(7, 72), n=c(613, 332))
test

#proportion of minority contestants per year from 2009 to 2019
mean(bachelor$minority[bachelor$year== 2009], na.rm= T)
years<- unique(bachelor$year)
years<- c(2009:2019)
results<- rep(NA, length(years)) 
names(results)<- years
for(i in 1: length(years)){
  results[i]<- mean(bachelor$minority[bachelor$year== years[i]], na.rm=T)
}
results

##graphing results
pdf("FinalProjectMinorityPerYear.pdf", width=6, height=5)
plot(x= years,
     y= results,
     xlim= c(2009, 2019),
     ylim= c(0, 0.4),
     type= "l",
     lwd= 3,
     xlab= "Years",
     ylab= "Mean Minority Contestants",
     main= "Mean Minority Contestants per Year Over Time",
     cex.main= .9, 
     col= "blue",
     xaxt= "n",
     bty= "n")
text(x= 2012, y= 0.3, "Lawsuit", cex= .8)
abline(v= 2012, col= "red")
axis(1, 2009:2019, tick = TRUE, cex.axis=1)
dev.off()

#proportion of minority leads per year from 2002 to 2019
mean(minoritylead[leadsdata$year== 2002])
leadyears<- unique(leadsdata$year)
leadresults<- rep(NA, length(leadyears)) 
names(leadresults)<- leadyears
for(i in 1: length(leadyears)){
  leadresults[i]<- mean(minoritylead[leadsdata$year== leadyears[i]])
}
leadresults

##graphing results
pdf("FinalProjectLeadPerYear.pdf", width=6, height=5)
plot(x= leadyears,
     y= leadresults,
     xlim= c(2002, 2019),
     type= "l",
     lwd= 3,
     xlab= "Years",
     ylab= "Mean Minority Leads",
     main= "Mean Minority Leads per Year Over Time",
     cex.main= .9, 
     col= "blue",
     xaxt= "n",
     bty= "n")
abline(v=2012, col= "red")
text(x= 2012, y= 0.3, "Lawsuit", cex= 0.8)
axis(1, 2002:2019, tick = TRUE, cex.axis=1)
dev.off()