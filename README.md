# appdensitycalculation
Data Science Toolbox Discussion Exercise

## Barangay Density Data

### Data Manipulation


    #Import CSV Files
    population = read.csv("population.csv")
    regionarea = read.csv("regionarea.csv")
  
    #Load dplyr library
    library("dplyr")
  
    #Count the Number of Barangay per Region
    NumofBrgy= count(population,Region)
    BarData = left_join(population, regionarea,   by=c("Region"))
    BarData = left_join(BarData, NumofBrgy, by=c("Region"))


Explanation:
We first got the number of barangays per region using *count* function. We used *leftjoin* to add the columns Area (from regionarea.csv) and Number of Barangay per Region (from count function).

### Barangay Area Calculation
Formula:
$$
Area\:of\:Barangay\:in\:Region\:1 = 
\frac{Area\:of\:Region\:1}{Number\:of\:Barangays\:in\:Region\:1}
$$
Code Snippet:

    #Divide the area by the number of barangays in the region
    BarData$BrgyArea = BarData$Area / BarData$n


Explanation:
In order to get the area per barangay, the total area of the region is divided by the total number of barangays in the region.

### Barangay Population Density Calculation
Formula:
$$
Barangay\:1\:Population\:Density = 
\frac{Population\:of\:Barangay\:1}{Area\:of\:Barangay\:1}
$$

Code Snippet:

    #get the population density
    BarData$Density = BarData$Population / BarData$BrgyArea


Explanation:
The barangay population density was calculated by dividing the barangay population by the barangay area calculated previously.

### Getting the Top 5 Population Densities for Barangays in the Philippines

    #outputs the top 5 barangays in the Philippines
    Top5BrgyPH = slice_max(BarData,Density,n=5)
    Top5BrgyPH

Explanation:
*slice_max* was used to get the 5 highest densities in the Philippines. 

Note: We also determined the Top 5 barangays per Region and per City.

## City Density Data

### Data Manipulation

    #Import CSV Files
    population = read.csv("population.csv")
    regionarea = read.csv("regionarea.csv")
    
    #Load dplyr library
    library("dplyr")
    
    #Count the Number of Cities per Region
    NumofCities= count(population,Region, CityProvince)
    NumOfCity = count(NumofCities, Region)
    
    #get the total population per city
    TotalPerCity = aggregate(Population ~ CityProvince, population, sum)
    
    #left join the count of the number of cities per region and the total population per city
    BarData = left_join(population, regionarea, by=c("Region"))
    BarData = left_join(BarData, NumOfCity, by=c("Region"))
    BarData = left_join(BarData, TotalPerCity, by=c("CityProvince"))

Explanation:
We first got the number of cities per region using the *count* function. We then calculated the total population per city using the *aggregate* command. Lastly, we used *leftjoin* to add the columns, Area (from regionarea.csv), Number of City (from count command), and Total Population (from aggregate command)

### City Area Calculation
Formula:
$$
Area\:of\:City\:in\:Region\:1 = 
\frac{Area\:of\:Region\:1}{Number\:of\:Cities\:in\:Region\:1}
$$
Code Snippet:

    BarData$CityArea = BarData$Area / BarData$n


Explanation:
In order to get the area per city, the total area of the region is divided by the total number of cities in the region.

### City Population Density Calculation
Formula:
$$
City\:1\:Population\:Density = 
\frac{Population\:of\:City\:1}{Area\:of\:City\:1}
$$

Code Snippet:

    BarData$Density = BarData$Population.y / BarData$CityArea


Explanation:
The city population density was calculated by dividing the city population by the city area calculated previously.

### Getting the Top 5 Population Densities for Cities in the Philippines


    #filter it to have just the city, region and density value
    TotalPerCity1 = aggregate(Density ~ CityProvince + Region ,BarData, mean)
    
    #output csv file with top 5 city population densities in the PH
    Top5CitiesPH = slice_max(TotalPerCity1,Density,n=5)
    Top5CitiesPH

Explanation:
*slice_max* was used to get the 5 highest densities in the Philippines. 

Note: We also determined the Top 5 cities per Region and per City.
