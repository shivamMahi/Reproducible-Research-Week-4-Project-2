install.packages("dplyr")
library(dplyr)
dataset<-download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","/cloud/project/storm_dataset")

storm_data <- read.csv(bzfile("storm_dataset"))
head(dataset)


str(dataset)
names(dataset)

#1)  Across the United States, which types of events (as indicated in the EVTYPE 
#variable) are most harmful with respect to population health?

#for Events we have to compare with 2 health problems i.e injuries and fatalities

#We have to aggregate both one by one the compare

#a) aggregating EVTYPE wrt injuries
total_injuries <- aggregate(INJURIES~EVTYPE, dataset, sum)
total_injuries <- arrange(total_injuries, desc(INJURIES))
total_injuries <- total_injuries[1:20, ]
total_injuries

#b) aggregating EVTYPE wrt fatalities
total_fatalities <- aggregate(FATALITIES~EVTYPE,dataset, sum)
total_fatalities <- arrange(total_fatalities, desc(FATALITIES))
total_fatalities <- total_fatalities[1:20, ]
total_fatalities

barplot(t(totals[,-1]))

par(mfrow = c(1, 2), mar = c(15, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(totals$FATALITIES, las = 3, names.arg = totals$EVTYPE, main = "Weather Events With\n The Top 10 Highest Fatalities", ylab = "Number of Fatalities", col = totals$FATALITIES)
barplot(totals$INJURIES, las = 3, names.arg =totals$EVTYPE, main = "Weather Events With\n The Top 10 Highest Injuries", ylab = "Number of Injuries", col = totals$FATALITIES)



#OR      ##########Creating double bar graphs

#c) merging both
totals<- merge(total_fatalities, total_injuries, by.x = "EVTYPE", by.y = "EVTYPE")
totals<-arrange(totals,desc(FATALITIES+INJURIES))

library(reshape2)
bad_stuff <- melt(totals, id.vars="EVTYPE", variable.name = "bad_thing")
tail(bad_stuff, 5)


#g <- ggplot(data = bad_stuff, aes(x = reorder(EVTYPE,-value), y = value))
#g <- g + geom_bar(stat = "identity", color = bad_stuff$EVTYPE) 
#g <- g + labs(title = "Total people loss in USA by weather events in 1996-2011")
#g <- g + theme(plot.title = element_text(hjust = 0.5))
#g <- g + labs(y = "Number of fatalities and injuries", x = "Event Type")
#g <- g + coord_flip() 
#print(g)

# Create chart
healthChart <- ggplot(bad_stuff, aes(x=reorder(EVTYPE, -value), y=value))

# Plot data as bar chart
healthChart = healthChart + geom_bar(stat="identity", aes(fill=bad_thing), position="dodge")

# Set x-axis label
healthChart = healthChart + xlab("Event Type") 

# Rotate x-axis tick labels 
healthChart = healthChart + theme(axis.text.x = element_text(angle=45, hjust=1))

# Set chart title and center it
healthChart = healthChart + ggtitle("Top 10 US Killers") + theme(plot.title = element_text(hjust = 0.5))

healthChart


#2. Across the United States, which types of events have the greatest economic 
#consequences?

# we have property Damage and crop damage 
# Aggregate Data for Property Damage
propdmg <- aggregate(PROPDMG ~ EVTYPE, data = dataset, FUN = sum)
propdmg <- propdmg[order(propdmg$PROPDMG, decreasing = TRUE), ]
# 10 most harmful causes of injuries
propdmgMax <- propdmg[1:10, ]
print(propdmgMax)


#Aggregate Data for Crop Damage
cropdmg <- aggregate(CROPDMG ~ EVTYPE, data = dataset, FUN = sum)
cropdmg <- cropdmg[order(cropdmg$CROPDMG, decreasing = TRUE), ]
# 10 most harmful causes of injuries
cropdmgMax <- cropdmg[1:10, ]
print(cropdmgMax)

par(mfrow = c(1, 2), mar = c(15, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)

barplot(propdmgMax$PROPDMG, las = 3, names.arg = propdmgMax$EVTYPE, 
        main = "Top 10 Events with\n Greatest Property Damages", 
        ylab = "Number of Injuries", col = propdmgMax$PROPDMG)

barplot(cropdmgMax$CROPDMG, las = 3, names.arg = cropdmgMax$EVTYPE, 
        main = "Top 10 Events with\n Greatest Crop Damages", 
        ylab = "Number of Injuries", col = cropdmgMax$CROPDMG)


########  MAKING DOUBLE BAR GRAPH
#merging both
totalDamage<- merge(propdmgMax,cropdmgMax,by.x = "EVTYPE", by.y = "EVTYPE")
totalDamage<-arrange(totalDamage,desc(PROPDMG + CROPDMG))


top_10_damages <- melt(totalDamage, id.vars="EVTYPE", variable.name = "Damage_Types")
head(top_10_damages, 5)

# Create chart
DamageChart <- ggplot(top_10_damages, aes(x=reorder(EVTYPE, -value/100000), y=value/100000))

# Plot data as bar chart
DamageChart = DamageChart + geom_bar(stat="identity", aes(fill=Damage_Types), position="dodge")

# Set x-axis label
DamageChart = DamageChart + xlab("Event Type") 

# Rotate x-axis tick labels 
DamageChart = DamageChart + theme(axis.text.x = element_text(angle=45, hjust=1))

# Set chart title and center it
DamageChart = DamageChart + ggtitle("Top 10 greatest economic consequences") + theme(plot.title = element_text(hjust = 0.5))

DamageChart

########  MAKING TRIPLE BAR GRAPH

#merging both
totalDamage<- merge(propdmgMax,cropdmgMax,by.x = "EVTYPE", by.y = "EVTYPE")
totalDamage$TOTALDMG <- totalDamage$PROPDMG + totalDamage$CROPDMG
totalDamage<-arrange(totalDamage,desc(TOTALDMG))
#totalDamage<-totalDamage[,c(totalDamage$EVTYPE,round(totalDamage$PROPDMG),round(totalDamage$CROPDMG),round(totalDamage$TOTALDMG))]

top_10_damages <- melt(totalDamage, id.vars="EVTYPE", variable.name = "Damage_Types")
tail(top_10_damages, 5)

# Create chart
DamageChart <- ggplot(top_10_damages, aes(x=reorder(EVTYPE, -value/1000), y=value/1000),fill=Damage_Types)

# Plot data as bar chart
DamageChart = DamageChart + geom_bar(stat="identity", aes(fill=Damage_Types), position="dodge")

# Set x-axis label
DamageChart = DamageChart + xlab("Event Type") + ylab("Cost of damage in $(billions)")

# Rotate x-axis tick labels 
DamageChart = DamageChart + theme(axis.text.x = element_text(angle=45, hjust=1))

# Set chart title and center it
DamageChart = DamageChart + ggtitle("Top 10 greatest economic consequences") + theme(plot.title = element_text(hjust = 0.5))

DamageChart

