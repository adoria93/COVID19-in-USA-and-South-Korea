library(tidyverse)

# COVID-19 Breakdown -- USA vs. South Korea

# These two countries provide us with an opportunity to see
# how each country responded to the outbreak. Both the United States
# and South Korea had their first reported case on the same day.

# Data used here has infromation until April 5, 2020.
# Dataset link: https://www.kaggle.com/imdevskp/corona-virus-report

# Importing the data
covid_19_clean_complete <- read.csv("~/Projects/04 USA-South Korea COVID-19/00 Data/covid_19_clean_complete.csv")

covid_19_clean_complete$Date <- as.Date(covid_19_clean_complete$Date, format = "%m/%d/%y")

# Overall cases
overall <- covid_19_clean_complete %>%
  group_by(Date) %>%
  summarize(total_confirmed = sum(Confirmed),
            total_deaths = sum(Deaths),
            total_recoveries = sum(Recovered))

write.csv(overall, "overall.csv")

# Latest Information
latest_info <- tail(overall, 1)

latest_info$death_rate <- latest_info$total_deaths / latest_info$total_confirmed
latest_info$recovery_rate <- latest_info$total_recoveries / latest_info$total_confirmed
write.csv(latest_info, "latest_info.csv")

ggplot(data = overall) +
  geom_line(aes(x = Date, y = total_confirmed, color = "red")) +
  geom_line(aes(x = Date, y = total_deaths, color = "blue")) +
  geom_line(aes(x = Date, y = total_recoveries, color = "green")) +
  scale_color_discrete(name = "Legend", labels = c("Deaths", "Recoveries", "Confirmed Cases")) +
  ggtitle("COVID-19 Worldwide") +
  xlab("Date") + ylab("Total")
            
# Create dataset with only USA and South Korea in it
usa_skorea <- filter(covid_19_clean_complete, `Country.Region` == 'US' |
                       `Country.Region` == 'South Korea')

usa_skorea_overall <- usa_skorea %>%
  group_by(Date, `Country.Region`) %>%
  summarize(total_confirmed = sum(Confirmed),
            total_deaths = sum(Deaths),
            total_recoveries = sum(Recovered))

write.csv(usa_skorea_overall, "USA_SKorea_overall.csv")

# Confirmed Cases
ggplot(data = usa_skorea_overall) +
  geom_line(aes(x = Date, y = total_confirmed, color = `Country.Region`)) +
  ggtitle("COVID-19 Confirmed Cases") +
  xlab("Date") + ylab("Total")

# Deaths
ggplot(data = usa_skorea_overall) +
  geom_line(aes(x = Date, y = total_deaths, color = `Country.Region`)) +
  ggtitle("COVID-19 Deaths") +
  xlab("Date") + ylab("Total")

# Recoveries
ggplot(data = usa_skorea_overall) +
  geom_line(aes(x = Date, y = total_recoveries, color = `Country.Region`)) +
  ggtitle("COVID-19 Recoveries") +
  xlab("Date") + ylab("Total")

#-----------------------------------

# Active cases are the cases that have
# been confirmed, but have not recovered or resulted in a death.

usa_skorea_overall$total_active_cases = usa_skorea_overall$total_confirmed - usa_skorea_overall$total_deaths - usa_skorea_overall$total_recoveries

# Calculating recovery and death rates
usa_skorea_overall$death_rate <- round((usa_skorea_overall$total_deaths / usa_skorea_overall$total_confirmed), 2)
usa_skorea_overall$recovery_rate <- round((usa_skorea_overall$total_recoveries / usa_skorea_overall$total_confirmed), 2)

# Subsetting the overall data to only include the date,
# country, death rate and recovery rate
usa_skorea_rates <- tail(usa_skorea_overall[c(1:2,7:8)], 2)

ggplot(data = usa_skorea_overall) +
  geom_line(aes(x = Date, y = total_active_cases, color = `Country.Region`)) +
  ggtitle("COVID-19 Active Cases") +
  xlab("Date") + ylab("Total")

# Because of the scale of the number of cases in the USA, it
# makes it look like South Korea didn't experience any sort of
# exponentail growth. We need to separate these two countries
# and put them into their own datasets

# PUtting each country into their own dataset
skorea_overall <- filter(usa_skorea_overall, `Country.Region` == "South Korea")
usa_overall <- filter(usa_skorea_overall, `Country.Region` == "US")

write.csv(skorea_overall, "skorea_overall.csv")
write.csv(usa_overall, "usa_overall.csv")

# South Korea
ggplot(data = skorea_overall) +
  geom_line(aes(x = Date, y = total_active_cases)) +
  ggtitle("COVID-19 Active Cases (South Korea)") +
  xlab("Date") + ylab("Total")

# USA
ggplot(data = usa_overall) +
  geom_line(aes(x = Date, y = total_active_cases)) +
  ggtitle("COVID-19 Active Cases (USA)") +
  xlab("Date") + ylab("Total")