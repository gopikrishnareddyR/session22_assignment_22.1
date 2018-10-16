#session22_assignment_22.1

getwd()
p<-"C:/Users/DELL/Documents"
setwd(p)
epi_r<-read.csv("C:/Users/DELL/Documents/epi_r.csv")

View(epi_r)
sum(is.na(epi_r))

###################################
epi_r<-epi_r[,1:6]
kmeans(epi_r,5)
View(epi_r)

complete.cases(epi_r)
nrow(epi_r[!complete.cases(epi_r),])
library(mice)
median(epi_r$calories, na.rm = TRUE)
mean(epi_r$calories, na.rm = TRUE)

median(epi_r$protein, na.rm = TRUE)
mean(epi_r$protein, na.rm = TRUE)






epi_rr<-epi_r
View(epi_rr)
library(VIM)
aggr_plot <- aggr(epi_rr, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(epi_rr), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
marginplot(epi_rr[c(1,2)])



library(mice)
md.pattern(epi_rr)
epi_rr <- mice(epi_rr,m=5,maxit=10,meth='pmm',seed=500)
summary(epi_rr)

library(lattice)
densityplot(epi_rr)

epi_rr$imp$protein

completed_epi<-complete(epi_rr,1)
View(completed_epi)
############################################


#a.Apply k means clustering to identify similar recepies
epi_r<-epi_r[,-c(1:6)]
epi_r<-scale(epi_r)
summary(epi_r)
epi_rr<-kmeans(epi_r,1)
epi_rr$withinss
epi_rr$tot.withinss

epi_rr<-kmeans(epi_r,2)
epi_rr$withinss
epi_rr$tot.withinss

epi_rr<-kmeans(epi_r,4)
epi_rr$iter
epi_rr$tot.withinss
summary(epi_rr)

epi_rr$centers

#b.Apply k means clustering to identify similar Attributes

#c.how many unique recepies that people order often

#d. what are their typical profiles


