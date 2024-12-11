#Installing the necessary libraries and loading
install.packages("ggplot2")
install.packages("maps")
install.packages("maptools")
install.packages("ggmap")
install.packages("gridExtra")
install.packages("scatterplot3d")


library(ggplot2)
library(maps)
library(ggmap)
library(gridExtra)
library(grid)
library(scatterplot3d)


#We are loading the dataset from online which do not require any libraries for loading the data
clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")
str(clim)

#To get an insight of what is wrong with “altitude” and “p_mean”.
#Viewing the data in tabular form
View(clim)
#Checking the names of the column in the data
names(clim)

#To figure out the problems with “altitude” and “p_mean”, let's check the structure individually.
str(clim$altitude)
str(clim$p_mean)

# Let's also check the classes for each of them
class(clim$altitude)
class(clim$p_mean)

#From the output gotten from the class, it shows that both are character class which is going to be difficult for R to deal with

# Using Unique funtion to check if there are other things wrong with the values in altitude
unique(clim$altitude)
unique(clim$p_mean)

#From the output, some numbers have commas separating them.

#To resolve the both issues found, we need to use gsub() to remove the commas and replace it with no space, and as.numeric() function to transform the factor to numeric
clim$altitude <- as.numeric(gsub(",", "", clim$altitude))
clim$p_mean <- as.numeric(gsub(",", "", clim$p_mean))

#Checking the structure again and unique values again
str(clim)
unique(clim$altitude)
unique(clim$p_mean)


#Before proceeding, ensuring that no missing value
clim <- na.omit(clim)
View(clim)

#We want to explain the temperature by spatial attributes using multiple linear regression. Let
#us plot the stations on a map to get an idea of their locations

#Plotting two different map on the same dataset to show different representation of the data using using ggmap and ggplot

#To use the ggmap, I registered for an api on https://client.stadiamaps.com/
register_stadiamaps(key = "759a0a43-c2c4-43cd-9da0-a9d9f550e72e")
# Get a map of France (centered and zoomed appropriately) using Stadia Maps
france_map <- get_stadiamap(
  bbox = c(left = -5, bottom = 41, right = 10, top = 52), # Bounding box for France
  zoom = 6,
  maptype = "stamen_terrain"  # Use a valid map type like "stamen_terrain"
)

# Plot the map with station points
france_map_ggmap <- ggmap(france_map) +
  geom_point(data = climfrar, aes(x = lon, y = lat), 
             color = "red", size = 2) +
  ggtitle("Climate Stations in France") +
  xlab("Longitude") +
  ylab("Latitude")




# Load the base map of France
france_map <- map_data("france")

# Plot the map and overlay the climate station locations
france_map_ggplot <- ggplot() +
      # Draw the France map polygons
      geom_polygon(data = france_map, aes(x = long, y = lat, group = group), 
                   fill = "lightblue", color = "black") +
      # Overlay station points
      geom_point(data = clim, aes(x = lon, y = lat), 
                 color = "red", size = 3) +
      # Add minimal theme and labels
      theme_minimal() +
      labs(title = "Climate Stations in France", x = "Longitude", y = "Latitude")


# Combine both plots in a grid
#grid.arrange(france_map_ggmap, france_map_ggplot, ncol = 2)

# Combine plots
combined_plot <- grid.arrange(
  france_map_ggmap, 
  france_map_ggplot, 
  ncol = 2,
  top = textGrob("Combined Climate Station Maps", gp = gpar(fontsize = 16, fontface = "bold"))
)


#Saving the plot
# Save to working memory
ggsave("combined_climate_station_maps.png", plot = combined_plot, width = 10, height = 6)


#Exercise 1: Test all three spatial attributes, i.e. latitude, longitude and altitude, as explanatory
#variables for the mean annual temperature

#Excluding the two high mountain extremes.
climfrar <- clim[1:34, ]
View(climfrar)

#Writing the linear model and interpreting the results

#This is a multiple linear regression problem, we will make use of the lm()
model <- lm(t_mean ~ altitude + lat + lon, data = climfrar)
model


#MODEL OUTPUT
#Call:
#  lm(formula = t_mean ~ altitude + lat + lon, data = climfrar)

#Coefficients:
#  (Intercept)     altitude          lat          lon  
#37.265036    -0.006414    -0.533960     0.032101  



#INTERPRETATION
#The regression model suggests that the mean temperature (t_mean) decreases by 
#approximately 0.0064°C per meter of altitude, decreases by 0.534°C per degree of latitude, 
#and increases by 0.032°C per degree of longitude, with an intercept of 37.27°C representing 
#the estimated mean temperature at sea level at the origin of latitude and longitude.

#Putting the generated coefficients based into multiple linear regression formula based on generated model output

#Before
#Mean Annual Temperature = … + (...) × altitude + (...) × latitude + (...) × longitude

#Now
#Mean Annual Temperature=37.265+(−0.006)×altitude+(−0.534)×latitude+0.032×longitude.


#Exercise 2: Exclude the non-significant variable and predict the temperature for
#Mont-Ventoux and Pic-du-midi. Compare the measured means concerning the prediction
#confidence bounces.

#To check the non-significant variable, we check the summary of the previous model
summary(model)

#OUTPUT

#Call:
#  lm(formula = t_mean ~ altitude + lat + lon, data = climfrar)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.76492 -0.32755  0.04337  0.24787  2.58927 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 37.2650364  2.6220099  14.212 7.29e-15 ***
#  altitude    -0.0064139  0.0008688  -7.383 3.17e-08 ***
#  lat         -0.5339603  0.0557546  -9.577 1.24e-10 ***
#  lon          0.0321010  0.0395728   0.811    0.424    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.7308 on 30 degrees of freedom
#Multiple R-squared:  0.8329,	Adjusted R-squared:  0.8162 
#F-statistic: 49.84 on 3 and 30 DF,  p-value: 9.112e-12

#INTERPRETATION
#The regression model explains 83.3% of the variability in mean temperature (t_mean) with altitude 
#and latitude having significant effects: temperature decreases by 0.0064°C per meter of altitude 
#and 0.534°C per degree of latitude (both p < 0.001). Longitude has no significant effect (p = 0.424), 
#and the residual standard error is 0.73°C, indicating a good fit overall.

#Since longitude has no significant effect, we adjust the model to the below
model2 <- lm(t_mean ~ altitude + lat, data = climfrar)
summary(model2)

#OUTPUT
#Call:
#lm(formula = t_mean ~ altitude + lat, data = climfrar)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.79206 -0.27571 -0.00556  0.30536  2.71871 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 37.9147567  2.4828724   15.27 5.68e-16 ***
#  altitude    -0.0062643  0.0008443   -7.42 2.34e-08 ***
#  lat         -0.5465325  0.0532610  -10.26 1.72e-11 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.7268 on 31 degrees of freedom
#Multiple R-squared:  0.8292,	Adjusted R-squared:  0.8182 
#F-statistic: 75.26 on 2 and 31 DF,  p-value: 1.268e-12


#INTERPRETATION
#By removing longitude, the adjusted model retains altitude and latitude as significant 
#predictors of mean temperature, explaining 82.9% of the variability (adjusted R² = 81.8%). 
#Temperature decreases by 0.0063°C per meter of altitude and 0.547°C per degree of latitude 
#(both p < 0.001), with a residual standard error of 0.73°C, indicating a strong fit similar 
#to the original model but with reduced complexity.

#The linear model will now be written as
#Mean Annual Temperature = 37.915 + (−0.006) × altitude + (−0.547) × latitude

#Now to predict the temperature for Mont-Ventoux and Pic-du-midi.
pred_t_mean <- predict(model2, newdata = list("altitude" = c(1212, 2860), "lat" = c(44.2, 42.9)), interval = "p", level = 0.95)
pred_t_mean


#OUTPUT
#      fit       lwr      upr
#1  6.165713  3.792341 8.539085
#2 -3.447331 -8.347964 1.453302

#INTEPRETATION
#The model predicts the mean temperature for Mont-Ventoux to be 6.17°C (95% prediction interval: 3.79°C to 8.54°C) 
#and for Pic-du-Midi to be -3.45°C (95% prediction interval: -8.35°C to 1.45°C). These predictions indicate lower 
#temperatures for higher altitudes, with wider prediction intervals reflecting greater uncertainty.

#COMPARISON
#The predicted mean for Mont-Ventoux is 6.17°C with a 95% prediction-interval of [3.79°C,8.54°C]. The measured mean is 3.6°C
#showing that the model prediction is not accurate enough to reproduce the temperature for Mont-Ventoux.
#Similarly, the predicted mean for Pic-du-midi is −3.45°C with a 95 prediction-interval of [−8.35°C,1.45°C]. 
#This shows that, the model prediction still covers the measured mean of −1.2°C


#Exercise 3: Evaluate the model results by i) a 3D-scatterplot and ii) by the summary output

#Model evaluation using 3D-scatterplot
# Set the file name and resolution 
png("scatterplot3d_plot.png", width = 800, height = 600)

# Generate the 3D scatter plot with the plane
scatter_3d <- with(climfrar, scatterplot3d(altitude, lat, t_mean,
                                           pch = 16, highlight.3d = TRUE,
                                           angle = 45))

# Add the plane from the model
scatter_3d$plane3d(model2)

# Save the plot to the file
dev.off()  # This closes the plotting device and saves the file

#INTERPRETATION
#Upon examining the 3D scatter plot, it is evident that the majority of the data points closely align with the plane, 
#with the exception of those with low altitude and low latitude. This suggests that the linear model does not provide 
#a good fit for all the data.


#Model evaluation using summary
summary(model2)


#INTERPRETATION
#The model explains 82.9% of the variance in the mean temperature (t_mean), with a high goodness of fit (adjusted R² = 81.8%) 
#and a highly significant overall model (F-statistic = 75.26, p < 0.001).

#Altitude and latitude both have statistically significant effects on mean temperature, with altitude decreasing temperature 
#by 0.0063°C per meter and latitude decreasing temperature by 0.547°C per degree (both p < 0.001).

#The residual standard error is 0.73°C, indicating that the model's predictions typically deviate from the observed temperatures 
#by this amount, and the residuals are relatively evenly distributed with a median close to zero.