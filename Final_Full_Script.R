

#installing and loading required packages and libraries

install.packages("tidyverse")
install.packages("ggpubr") # later used for linear correlation statistics
install.packages("ggpmisc") #later used for polynomial statistics

library(tidyverse)
library(gridExtra) # used for plotting
library(ggpubr)
library(ggpmisc)



#getting the dataset without pm values for 2023

daily_no_PM2023 <- read_csv("Coursera/Google/Data Analytics Certificate/Case Study -Capstone Project/Track B - Own/R/daily_no_PM2023.csv")


#fixing stations to match pm's data stations so i can merge 


daily_no_PM2023 <- daily_no_PM2023 %>% 
  separate(Station_name,into = c("Station_Location", "Station_Type","Xtra"), sep = " - ")

daily_no_PM2023 <- daily_no_PM2023 %>%
  mutate(Station_Location = if_else(Station_Location == "EMEP", paste(Station_Location, Station_Type, sep = " - "), Station_Location))

daily_no_PM2023 <- daily_no_PM2023 %>%
  mutate(Station_Type = if_else(Station_Type == "Agia Marina Xyliatou", Xtra, Station_Type))

daily_no_PM2023 <- daily_no_PM2023 %>% 
  select(-Xtra)

daily_no_PM2023$Date<-dmy(daily_no_PM2023$Date)

#columns are done

#pivoting to wide format to match PM data format

pivoted_df <- pivot_wider(daily_no_PM2023, 
                          id_cols = c(Date, Station_Location, Station_Type),
                          names_from = Pollutant,
                          values_from = Avg_Daily_Value)


#dropping PM data since we have the PM data from 2018-2023 in another file and CO data since i found out is not needed but i have it from the mistakes
#i made before

pivoted_df <- pivoted_df %>% 
  select(-"Particulate Matter <10 micron (PM10)",-"Particulate Matter <2.5 micron (PM2.5)",-"Carbon Monoxide (CO)")




#uploading the new PM files

PM10_Data_2023 <- read_csv("Coursera/Google/Data Analytics Certificate/Case Study -Capstone Project/Track B - Own/R/PM10_Data_2023.csv")

PM2_5_Data_2018_2023 <- read_csv("Coursera/Google/Data Analytics Certificate/Case Study -Capstone Project/Track B - Own/R/PM2.5_Data_2018-2023.csv")



#correcting the format of each file to match the format of the other files
pm10_long_full <- PM10_Data_2023 %>%
  pivot_longer(cols = c(`Nicosia - Traffic Station`, `Limassol - Traffic Station`, 
                        `Larnaka - Traffic Station`, `Paralimni - Traffic Station`, 
                        `Zygi - Industrial Station`, `EMEP - Agia Marina Xyliatou - Background Station`, 
                        `Pafos - Traffic Station`),
               names_to = "Station", values_to = "Particulate Matter <10 micron (PM10)")


pm2.5_long_full <- PM2_5_Data_2018_2023 %>%
  pivot_longer(cols = c(`Nicosia - Traffic Station`, `Limassol - Traffic Station`, 
                        `Larnaka - Traffic Station`, `Paralimni - Traffic Station`, 
                        `Zygi - Industrial Station`, `EMEP - Agia Marina Xyliatou - Background Station`, 
                        `Pafos - Traffic Station`),
               names_to = "Station", values_to = "Particulate Matter <2.5 micron (PM2.5)")


#combining the pollutants into one dataframe

pm_data_full <- left_join(pm10_long_full, pm2.5_long_full, by = c("Date", "Station"))

#fixing the station columns to match the other files

pm_data_full <- pm_data_full %>% 
  separate(Station,into = c("Station_Location", "Station_Type","Xtra"), sep = " - ")

pm_data_full <- pm_data_full %>%
  mutate(Station_Location = if_else(Station_Location == "EMEP", paste(Station_Location, Station_Type, sep = " - "), Station_Location))


pm_data_full <- pm_data_full %>%
  mutate(Station_Type = if_else(Station_Type == "Agia Marina Xyliatou", Xtra, Station_Type))

pm_data_full <- pm_data_full %>% 
  select(-Xtra)



#making sure column types match
str(pivoted_df)
str(pm_data_full)

pm_data_full$Date <- dmy(pm_data_full$Date)

pm_data_full$`Particulate Matter <10 micron (PM10)` <- as.numeric(pm_data_full$`Particulate Matter <10 micron (PM10)`)
pm_data_full$`Particulate Matter <2.5 micron (PM2.5)` <- as.numeric(pm_data_full$`Particulate Matter <2.5 micron (PM2.5)`)

str(pivoted_df)
str(pm_data_full)


daily_final_wide <- merge(pm_data_full,pivoted_df, by =c("Date","Station_Location","Station_Type"),all = TRUE)
View(daily_final_wide)

#rounding all values into 3 decimals for consistency

numeric_col <- sapply(daily_final_wide,is.numeric)
daily_final_wide[numeric_col] <- round(daily_final_wide[numeric_col],3)
View(daily_final_wide)

#removing the extra df
remove(daily_no_PM2023,numeric_col,pm_data_full,pivoted_df)


#converting to long - to have just in case - might be more useful when visualizing
daily_final_long <- pivot_longer(daily_final_wide,cols = c("Particulate Matter <10 micron (PM10)",
                                                           "Particulate Matter <2.5 micron (PM2.5)",
                                                           "Nitrogen Dioxide (NO2)",
                                                           "Sulfur Dioxide (SO2)",
                                                           "Ozone (O3)"),
                                 names_to = "Pollutant", values_to = "Value")

#saving the 2 finshed files 
write.csv(daily_final_long,"Coursera/Google/Data Analytics Certificate/Case Study -Capstone Project/Track B - Own/finalized_dfs/from R/final_long.csv")

write.csv(daily_final_wide,"Coursera/Google/Data Analytics Certificate/Case Study -Capstone Project/Track B - Own/finalized_dfs/from R/final_wide.csv")


#i fixed in excel 2 new columns - Index_Value and Pollutant of Index (it was easier for me using MAX and INDEX-MATCH functions)
#so i upload the corrected file to apply loops to fix the last column Index_Level

final_wide <- read_csv("Coursera/Google/Data Analytics Certificate/Case Study -Capstone Project/Track B - Own/R/final_wide.csv")

#checking all columns are correct data types and fixing them

str(final_wide)

#dates were wrong
final_wide$Date <- dmy(final_wide$Date)
str(final_wide) # now are correct


#creating the index limits and levels

no2_limits <- c(40, 90, 120, 230, 340, 1000)
ozone_limits <- c(50, 100, 130, 240, 380, 800)
pm10_limits <- c(20, 40, 50, 100, 150, 1200)
pm25_limits <- c(10, 20, 25, 50, 75, 800)
so2_limits <- c(100, 200, 350, 500, 750, 1250)
list_of_level <- c("Very good","Good","Medium","Poor","Very poor","Extremely poor")

# Creating a named list of limits
index_limits <- list("Ozone (O3)" = ozone_limits,
                     "Nitrogen Dioxide (NO2)" = no2_limits,
                     "Sulfur Dioxide (SO2)" = so2_limits,
                     "Particulate Matter <10 micron (PM10)" = pm10_limits,
                     "Particulate Matter <2.5 micron (PM2.5)" = pm25_limits)


# Iterate over each row of wide_1
for (i in 1:nrow(final_wide)) {
  
  # Get the index type
  index_type <- final_wide$Pollutant[i]
  
  # Check if the index type exists in the index_limits
  if (index_type %in% names(index_limits)) {
    
    # Assign the limits
    limits <- index_limits[[index_type]]
    
    # Iterate over the limits
    for (j in 1:length(limits)) {
      
      # Check if the Index_Value is less than the current limit
      if (final_wide$Index_Value[i] < limits[j]) {
        
        # Assign the corresponding index level
        final_wide$Index_Level[i] <- list_of_level[j]
        break
      }
    }
  }
}

#saving the completed file
write.csv(final_wide,"Coursera/Google/Data Analytics Certificate/Case Study -Capstone Project/Track B - Own/finalized_dfs/from R/final_wide_full.csv")






# Creating a new df to dont mess up the finilized df while plotting

wide_2 <- final_wide

# adjusting column names to plot easier
names(wide_2)[names(wide_2) == "Particulate Matter <10 micron (PM10)"] <- "PM10"
names(wide_2)[names(wide_2) == "Particulate Matter <2.5 micron (PM2.5)"] <- "PM2_5"
names(wide_2)[names(wide_2) == "Nitrogen Dioxide (NO2)"] <- "NO2"
names(wide_2)[names(wide_2) == "Sulfur Dioxide (SO2)"] <- "SO2"
names(wide_2)[names(wide_2) == "Ozone (O3)"] <- "O3"


# Filtering out numeric columns
numeric_vars <- sapply(wide_2, is.numeric)

# Creating scatter plots with linear regression testing each each pollutant with Index Value
plots_list <- lapply(names(wide_2)[numeric_vars], function(var) {
  ggplot(wide_2, aes_string(x = "Index_Value", y = var, color = var)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "black") + # applying linear regression
    stat_cor(method = "pearson", label.x = 320, label.y = 10) + # displaying R-squared using pearson correlation
    labs(title = paste("Scatter plot of Index Value vs", var),
         x = "Index_Value", y = var)
})

# Plotting and saving all linear regression scatter plots 
gridExtra::grid.arrange(grobs = plots_list, ncol = 2)
ggsave()



# Creating scatter plots with 2nd order polynomial regression testing each each pollutant with Index Value
plots_list_poly <- lapply(names(wide_2)[numeric_vars], function(var) {
  ggplot(wide_2, aes_string(x = "Index_Value", y = var, color = var)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "black") + # applying 2nd order polynomial 
    stat_poly_eq(formula = y ~ poly(x, 2), 
                 label.x = "right", label.y = "bottom", 
                 parse = TRUE, 
                 aes(label = paste("R2 = ", round(..r.squared.., digits = 3), sep = ""))) + # displaying R squared
    labs(title = paste("Scatter plot of Index Value vs", var),
         x = "Index_Value", y = var)
})

# Plotting and saving all 2nd order polynomial regression scatter plots
gridExtra::grid.arrange(grobs = plots_list_poly, ncol = 2)
ggsave()



