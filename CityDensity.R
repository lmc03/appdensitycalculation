#Import CSV Files
population = read.csv("population.csv")
regionarea = read.csv("regionarea.csv")

#Load dplyr library
library("dplyr")

#Count the Number of Cities per Region
NumofCities= count(population,Region, CityProvince)
NumOfCity = count(NumofCities, Region)
NumOfCity

#get the total population per city
TotalPerCity = aggregate(Population ~ CityProvince, population, sum)
TotalPerCity

#left join the count of the number of cities per region and the total population per city
BarData = left_join(population, regionarea, by=c("Region"))
BarData = left_join(BarData, NumOfCity, by=c("Region"))
BarData = left_join(BarData, TotalPerCity, by=c("CityProvince"))
BarData

BarData$CityArea = BarData$Area / BarData$n
BarData$Density = BarData$Population.y / BarData$CityArea

#filter it to have just the city, region and density value
TotalPerCity1 = aggregate(Density ~ CityProvince + Region ,BarData, mean)

#output csv file with top 5 city population densities in the PH
Top5CitiesPH = slice_max(TotalPerCity1,Density,n=5)
Top5CitiesPH
write.csv(Top5CitiesPH, "top5citiesph.csv")

#output csv file with top 5 city population densities per region
Top5PerRegion = TotalPerCity1[order(TotalPerCity1$Density, decreasing = TRUE), ]  # Order data descending
Top5PerRegion = Reduce(rbind,                                 # Top N highest values by group
                       by(Top5PerRegion,
                          Top5PerRegion["Region"],
                          head,
                          n = 5))
Top5PerRegion

write.csv(Top5PerRegion, "Top5citiesperregion.csv")
