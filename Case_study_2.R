install.packages("tidyverse")
install.packages("readr")
library(tidyverse)
library(readr)
daily_activity <- read.csv("02_Cleaned_data/dailyActivity_merged_clean.csv")
colnames(daily_activity)
## Calculating the per day average minutes of different intensities level

activity_summary_table <- daily_activity %>% summarise(average_very_active_minutes = mean(VeryActiveMinutes), average_fairly_active_minutes = mean(FairlyActiveMinutes), average_lightly_active_minutes = mean(LightlyActiveMinutes), average_sedantry_minutes = mean(SedentaryMinutes))
print(activity_summary_table)
glimpse(daily_activity)
head(daily_activity)
write.csv(activity_summary_table,"activity_summary_table.csv")

# Calculating average minutes of different intensities level: Weekend vs Weekday

daily_activity_days <- daily_activity %>% mutate(ActivityDate = as.Date(ActivityDate), day_of_week = weekdays(ActivityDate), day_type = if_else(day_of_week %in% c("Saturday", "Sunday"),"Weekend", "Weekday"))
## Categorized the ActivityDate column in Weekend and Weekday

daily_activity_days_summary <- daily_activity_days %>% group_by(day_type) %>% summarise(average_very_active_minutes = mean(VeryActiveMinutes), average_fairly_active_minutes = mean(FairlyActiveMinutes), average_lightly_active_minutes = mean(LightlyActiveMinutes), average_sedentary_minutes = mean(SedentaryMinutes))
write.csv(daily_activity_days_summary,"activity_summary_table_weekday_vs_weekend.csv")

## Calculating the overall average steps and calories

Average_steps_calories <- daily_activity %>% summarise(average_total_steps = mean(TotalSteps), average_calories_burned = mean(Calories))
write.csv(Average_steps_calories, "overall_average_of_total_steps_and_calories.csv")

#Making a plot showing relation of Total Steps and Number of calories burned in a day


library(ggplot2)
ggplot(data = daily_activity, mapping = aes(x = TotalSteps, y = Calories)) + geom_point() + geom_smooth() + labs(title =  "Daily Activity: Total Steps vs. Calories Burned", x = "Total Daily Steps", y = "Calories_Burned", caption = "Data Source : Fitbit Tracker Data via Kaggle")


## Saving the plot

ggsave(filename = "03_Visualizations/01_steps_vs_calories.png")

## Merging Daily dailySleep_summary with dailyActivity_merged_clean to discover trends

install.packages("janitor")
library(janitor)

# Importing the file 

daily_sleep <- read.csv("02_Cleaned_data/dailySleep_Summary.csv")

# Cleaning the column names of both datasets

daily_activity_clean <- daily_activity %>% clean_names()
daily_sleep_clean <- daily_sleep %>% clean_names()

# Merging the datasets based on IDs and Dates and saving it

merged_data <- daily_activity_clean %>% inner_join(daily_sleep_clean, by = c("id" = "id", "activity_date" = "sleep_date" ))
write.csv(merged_data, "02_Cleaned_data/Daily_activity_sleeep_merged.csv")

merged_data %>% select(total_steps, sedentary_minutes, total_minutes_asleep) %>% summary()

# Finding Out How Sedentary Time Spent In a Day Effects The Sleep

ggplot(data = merged_data, mapping = aes(x = sedentary_minutes, y = total_minutes_asleep)) + geom_point(color = "darkblue") + geom_smooth(color = "red") + labs(title = "Does Sitting All Day Effect Sleep?", x = "Sedentary Minutes", y = "Total Minutes Asleep", caption = "Data Source : Fitbit Tracker Data via Kaggle")
ggsave(filename = "04_Visualizations/totalminutesasleep_vs_sedentary_minutes.png")

# FInding relation between TOtal steps and Total Minutes Asleep

ggplot(data = merged_data, mapping = aes(x = total_steps, y = total_minutes_asleep)) + geom_point(color = "blue") + geom_smooth(color = "green") + labs(title = "Do More Daily Steps Lead to Better Sleep", x = "Total Steps", y = "Total Minutes Asleep", caption = "Data Source : Fitbit Tracker Data via Kaggle")
ggsave(filename = "04_Visualizations/totalsteps_vs_minutes_asleep.png")
