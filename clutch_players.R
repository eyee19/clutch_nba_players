#NBA clutch players
getwd()
setwd("C:/Users/Everett/Desktop/R")
dat <- read.csv("shot_logs.csv")
absDat <- abs(dat$FINAL_MARGIN) #need to change the negative final margins to positive, in this context changing it doesn't make a difference
dat$FINAL_MARGIN <- absDat
dat$GAME_CLOCK <- as.numeric(dat$GAME_CLOCK) #changing time to numeric value
names(dat)
dat[1742,8] #487 is the value that equals 7:00 minutes

#FinalDat is rows that are in the 4th quarter, point difference is 5 or less, and the game clock is between 7 and 12 minutes
finalDat <-dat[(dat$FINAL_MARGIN <= 5 & dat$PERIOD == "4" & dat$GAME_CLOCK >= 487),] #2709 rows
options(scipen = 999)
cleanedDat <- read.csv("top50.csv")
names(cleanedDat)
options(max.print=10000)
logreg <- glm(FGM ~ .-PTS-CLOSEST_DEFENDER-PERIOD, data = cleanedDat, family = "binomial")
summary(logreg)
sorted <- sort(logreg)

sink("logreg.txt") #this code just exports the summary output to a text file
print(summary(glm(FGM ~ .-PTS-CLOSEST_DEFENDER, data = cleanedDat, family = "binomial")))
sink()  

log.pred <- predict(logreg, cleanedDat, type = "response")
log.pred
yhat.test.class <- ifelse(log.pred>0.5, 1, 0) 
tab2.logreg <- table(cleanedDat$FGM, yhat.test.class) #confusion matrix
tab2.logreg
err2.logreg <- (515+366)/sum(tab2.logreg)
err2.logreg #32.8% error rate 
names(cleanedDat)

d <- density(cleanedDat$CLOSE_DEF_DIST) # returns the density data for defender distance in clutch time
plot(d, main = "KDE Defender Distance", xlab = "Distance in Feet")
polygon(d, col="blue", border="blue")

d2 <- density(cleanedDat$TOUCH_TIME) # returns the density data for touch time in clutch time
plot(d2, main = "KDE Touch Time", xlab = "Time in Seconds")
polygon(d2, col="blue", border="blue")

d1 <- density(cleanedDat$SHOT_DIST) # returns the density data for shot distance
plot(d1, main = "KDE Shot Distance", xlab = "Distance in Feet")
polygon(d1, col="blue", border="blue")

omitteddat <- na.omit(cleanedDat)
d3 <- density(omitteddat$SHOT_CLOCK) # returns the density data for shot clock in clutch time
plot(d3, main = "KDE Shot Clock", xlab = "Time in Seconds")
polygon(d3, col="blue", border="blue")

top50 <- read.csv("top50.csv")
names(top50)
logreg50 <- glm(FGM ~ .-PTS-CLOSEST_DEFENDER-PERIOD, data = top50, family = "binomial")
summary(logreg50)
