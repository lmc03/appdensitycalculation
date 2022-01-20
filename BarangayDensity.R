#Import CSV Files
population = read.csv("population.csv")
regionarea = read.csv("regionarea.csv")

#Load dplyr library
library("dplyr")

#Count the Number of Barangay per Region
NumofBrgy= count(population,Region)
BarData = left_join(population, regionarea, by=c("Region"))
BarData = left_join(BarData, NumofBrgy, by=c("Region"))

#Divide the area by the number of barangays in the region
BarData$BrgyArea = BarData$Area / BarData$n

#get the population density
BarData$Density = BarData$Population / BarData$BrgyArea

#outputs the top 5 barangays in the Philippines
Top5BrgyPH = slice_max(BarData,Density,n=5)
Top5BrgyPH

#outputs a csv file with the top 5 barangays per Region
Top5PerRegion = BarData[order(BarData$Density, decreasing = TRUE), ]  # Order data descending
Top5PerRegion = Reduce(rbind,                                
                        by(Top5PerRegion,
                           Top5PerRegion["Region"],
                           head,
                           n = 5))
Top5PerRegion
write.csv(Top5PerRegion,"Top5BrgyPerRegion.csv")

#outputs a CSV file with the top 5 barangays per city
Top5PerCity = BarData[order(BarData$Density, decreasing = TRUE), ]  # Order data descending
Top5PerCity = Reduce(rbind,                                
                      by(Top5PerCity,
                         Top5PerCity["CityProvince"],
                         head,
                         n = 5))
Top5PerCity
write.csv(Top5PerCity,"Top5BrgyPerCity.csv")