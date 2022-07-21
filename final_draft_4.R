########################################################
####### STA 404 Final Draft
#### Bich Ha Nguyen
#### Chicago Crime Dataset
#### Chicago - "The Crime City"? An analysis on the change in crime data in Chicago from past to present 
#### Last Updated: 11/15/2021


# download packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(ggmosaic)
library(GGally)
library(ggridges)
library(ggrepel)
library(scales)
library(sf)
library(ggwordcloud)
library(plotly)



# set working directory
setwd('C:/Users/77 thaiha/Pictures/2021F/STA404/Project/solo')

# read data
raw_data = read_csv('crimes.csv')
save(raw_data, file="crimes_raw.RData")

# load data
load("crimes_raw.RData")

## Step 1: Data Processing
summary(raw_data)
# columns with no NAs: ID, Case Number, Date, Block, IUCR, Primary Type, Location Description, Arrest, Domestic, Beat, District, Year, Location
# columns with NAs: Ward, Community Area, X, Y Coordinate, Latitude, Longitude



## Step 2: Making Plots


## Plot 1: Overall change throughout all years
yearly_data <- raw_data %>% group_by(Year) %>% summarize(total = n())
ggplot(data = yearly_data, aes(x=Year, y = total)) +
  geom_segment(
    aes(x=Year, xend=Year, y=0, yend=total), 
    color=case_when(yearly_data$Year < 2015 ~ "grey",
                    yearly_data$Year < 2019 ~ "wheat",
                    TRUE ~ "orange"), 
    size=ifelse(yearly_data$Year > 2014, 1.3, 0.7)) +
  geom_point(
    color=case_when(yearly_data$Year < 2015 ~ "grey",
                    yearly_data$Year < 2019 ~ "wheat",
                    TRUE ~ "orange"), 
    size=ifelse(yearly_data$Year > 2014, 5, 2)) +
  labs(x="", y="",
       title="Total Crimminal Accidents in Chicago (2001 - Present)",
       subtitle="Crime cases recorded by hundred thousands",
       caption="Source: Chicago Police Department") +
  scale_x_continuous(breaks=seq(2001, 2021, 2)) +
  scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3)) + 
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

help(case_when)
colnames(raw_data)
View(raw_data %>% select(`Primary Type`) %>% distinct())

## Plot 2: Changes in the past 5 years
past5 <- raw_data %>% 
  filter(Year > 2014, !(`Primary Type` %in% 
                          c("OTHER OFFENSE", "NON - CRIMINAL", "NON-CRIMINAL","NON-CRIMINAL (SUBJECT SPECIFIED)")))
past5$`Primary Type`[past5$`Primary Type` == "CRIM SEXUAL ASSAULT"] <- "CRIMINAL SEXUAL ASSAULT"

sum5 <- past5 %>% group_by(Year, `Primary Type`) %>% summarize(total = n())

# Not use this graph
ggplot() +
  geom_line(data = sum5, aes(x = Year, y = total, group = `Primary Type`),
            color="grey", alpha=0.5, size = 0.7) +
  geom_point(data = sum5, aes(x = Year, y = total, group = `Primary Type`),
             color="grey", alpha=0.5,size = 1) +
  geom_line(data = sum5 %>% filter(`Primary Type` %in% c("HOMICIDE", "HUMAN TRAFFICKING")), 
            aes(x = Year, y = total, group = `Primary Type`),
            color="tomato", alpha=0.5, size = 1.3) +
  geom_point(data = sum5 %>% filter(`Primary Type` %in% c("HOMICIDE", "HUMAN TRAFFICKING")), aes(x = Year, y = total, group = `Primary Type`),
             color="tomato", alpha=0.5,size = 2) + 
  labs(x="", y="",
       title="Crimminal Accidents by Type in Chicago (2015-2021)",
       subtitle="Total accidents on logarithm transformed scale",
       caption="Source: Chicago Police Department") +
  scale_x_continuous(breaks=seq(2015, 2021, 1)) +
  scale_y_log10() + 
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Plot 2.1: Change in past 2 years
past2 <- past5 %>% group_by(Year, `Primary Type`) %>% 
  summarize(total = n()) %>% filter(Year > 2018)

e <- geom_label_repel(data = past2 %>% filter(Year == 2021, 
                                        `Primary Type` %in% c("HOMICIDE", "HUMAN TRAFFICKING", "WEAPONS VIOLATION", "STALKING"))
                , aes(x= Year, y=total, label = `Primary Type`), size = 3, nudge_x = 1)
  

p1 <- ggplot() +
  geom_line(data = past2, aes(x = Year, y = total, group = `Primary Type`),
            color="grey", alpha=0.5, size = 1) +
  geom_point(data = past2, aes(x = Year, y = total, group = `Primary Type`),
             color="grey", alpha=0.5,size = 1.2) +
  geom_line(data = past2 %>% filter(`Primary Type` %in% c("HOMICIDE", "HUMAN TRAFFICKING", "WEAPONS VIOLATION", "STALKING")), 
            aes(x = Year, y = total, group = `Primary Type`),
            color="tomato", alpha=0.5, size = 1.5) +
  geom_point(data = past2 %>% filter(`Primary Type` %in% c("HOMICIDE", "HUMAN TRAFFICKING", "WEAPONS VIOLATION", "STALKING")), 
             aes(x = Year, y = total, group = `Primary Type`),
             color="tomato", alpha=0.5,size = 2) +
  labs(x="", y="",
       title="Crimminal Accidents by Type in Chicago during 2019 - 2021",
       subtitle="Total accidents on logarithm transformed scale",
       caption="Source: Chicago Police Department") +
  scale_x_continuous(breaks=seq(2019, 2021, 1)) +
  scale_y_log10() + 
  e +
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) 


ggplotly(p1)

## Plot 3: Change between time elements for increasing groups
past2 <- past5 %>% filter(Year > 2018, `Primary Type` %in% c("HOMICIDE", "WEAPONS VIOLATION")) %>%
  mutate(Date.Time = mdy_hms(Date),
         Year = factor(year(Date.Time)),
         Month = factor(month(Date.Time)),
         Day = factor(day(Date.Time)),
         Weekday =  factor(wday(Date.Time)),
         Hour =  factor(hour(Date.Time)))

# remove human trafficking, stalking
# investigate weapons violation and homicide

## Plot 3.1: Distribution by days in week
ggplot(data = past2, aes(x = Weekday, y = `Primary Type`, group=`Primary Type`)) +
  geom_density_ridges2(aes(fill=`Primary Type`), stat = "binline",
                       bins = 7, scale=0.95) + 
  geom_text(stat = "count", aes(y = group + 0.1*stat(count/max(count)) ,
                                label = ifelse(stat(count) > 0, stat(count), "")),
            vjust = -2.85, size = 4, color = "white" ) + 
  scale_fill_cyclical(values = c("#0000B0", "#7070D0")) +
  labs(x="", y="", 
       title = "Accidents Distribution by Type through Days in Week in 2020",
       subtitle = "Distribution of Homicide and Weapons Violation Offenses",
       caption="Source: Chicago Police Department") +
  theme_ridges(grid = FALSE)

## Plot 3.2: Distribution by Month
ggplot(data = past2, aes(x = Month, y = `Primary Type`, group=`Primary Type`)) +
  geom_density_ridges2(aes(fill=`Primary Type`), stat = "binline",
                       bins = 12, scale=0.95) + 
  geom_text(stat = "count", aes(y = group + 0.1*stat(count/max(count)) ,
                                label = ifelse(stat(count) > 0, stat(count), "")),
            vjust = -2.85, size = 4, color = "white" ) + 
  scale_fill_cyclical(values = c("#0000B0", "#7070D0")) +
  labs(x="", y="", 
    title = "Accidents Distribution by Type through month in 2020",
    subtitle = "Distribution of Homicide and Weapons Violation Offenses",
    caption="Source: Chicago Police Department") +
  theme_ridges(grid = FALSE)


## Plot 3.3: Distribution by Hour
ggplot(data = past2, aes(x = Hour, y = `Primary Type`, group=`Primary Type`)) +
  geom_density_ridges2(aes(fill=`Primary Type`), stat = "binline",
                       bins = 24, scale=0.95) + 
  geom_text(stat = "count", aes(y = group + 0.1*stat(count/max(count)) ,
                                label = ifelse(stat(count) > 0, stat(count), "")),
            vjust = -0.4, size = 3.75, color = "white" ) + 
  scale_fill_cyclical(values = c("#0000B0", "#7070D0")) +
  labs(x="", y="", 
       title = "Accidents Distribution by Type through Hour in 2020",
       subtitle = "Distribution of Homicide and Weapons Violation Offenses",
       caption="Source: Chicago Police Department") +
  theme_ridges(grid = FALSE)

## Plot 4: Dangerous places

# plot 4.1: count by type
past5 %>% filter(`Primary Type` %in% c("WEAPONS VIOLATION"), Year == 2020) %>%
  group_by(`Primary Type`,`Location Description`) %>%
  summarize(total_count = n()) %>%
  mutate(prop = round(total_count/sum(total_count)*100, 2)) %>%
  arrange(desc(prop)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(`Location Description`, total_count), y=total_count)) +
  geom_bar(fill="gold1", alpha = 0.8, stat="identity", show.legend = FALSE) +
  geom_text(aes(x=`Location Description`, label=prop),hjust=-0.1, size=3, color="slategray") +
  coord_flip() +
  theme_minimal() +
  labs(x="", y="", 
       title = "10 Locations with Highest Weapons Violation Accidents",
       subtitle = "Total Reported Case in 2020 by locations and their proportions",
       caption="Source: Chicago Police Department") +
  guides(legend = "none") +
  theme(
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))

past5 %>% filter(`Primary Type` %in% c("HOMICIDE"), Year == 2020) %>%
  group_by(`Primary Type`,`Location Description`) %>%
  summarize(total_count = n()) %>%
  mutate(prop = round(total_count/sum(total_count)*100, 2)) %>%
  arrange(desc(prop)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(`Location Description`, total_count), y=total_count)) +
  geom_bar(fill="purple4", alpha = 0.8, stat="identity", show.legend = FALSE) +
  geom_text(aes(x=`Location Description`, label=prop),hjust=-0.1, size=3, color="slategray") +
  coord_flip() +
  theme_minimal() +
  labs(x="", y="", 
       title = "10 Locations with Highest Homicide Accidents",
       subtitle = "Total Reported Case in 2020 by locations and their proportions",
       caption="Source: Chicago Police Department") +
  guides(legend = "none") +
  theme(
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))
  
  
# Plot 4.2: word generator
past5 %>% filter(`Primary Type` %in% c("HOMICIDE", "WEAPONS VIOLATION"), Year == 2020) %>%
  mutate(Street =  str_sub(Block, start = 7)) %>%
  group_by(Street) %>%
  summarize(total_count = n()) %>%
  arrange(desc(total_count)) %>%
  slice(1:50) %>%
  ggplot(aes(label = Street, size = total_count)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 8) +
  theme_minimal() +
  labs(
       title = "50 Most Common Streets with Homicide or Weapons Violation Accidents",
       subtitle = "Total Reported Case in 2020 by Street",
       caption="Source: Chicago Police Department")

## Plot 5: Cloropleth the distribution of crime in the areas

summary(past2)
View(past2 %>% filter(is.na(Latitude), is.na(Longitude)))

# download data
chi_areas <- read_sf("Boundaries - Community Areas (current).geojson")

# sample data
head(chi_areas)
ggplot(data = chi_areas) +
  geom_sf() +
  geom_sf_text(aes(label = area_num_1), size = 2) +
  theme_bw()
View(chi_areas)


# map 1
past5 %>% filter(`Primary Type` == "WEAPONS VIOLATION", Year == 2020) %>%
  group_by(`Community Area`) %>%
  summarize(total_count = n()) %>%
  mutate(`Community Area` = factor(`Community Area`)) %>%
  right_join(chi_areas, by=c("Community Area" = "area_num_1")) %>%
  ggplot(aes(geometry = geometry, fill = total_count)) + 
  geom_sf() +
  scale_fill_viridis_c(name="Total Cases", alpha = .5) +
  geom_sf_text(aes(label = `Community Area`), size = 2) +
  theme_minimal() +
  labs(x="Latitude", y="Longitude", 
       title = "Weapons Violation by Community Areas in Chicago",
       subtitle = "Total Reported Case in 2020",
       caption="Source: Chicago Police Department")

# map 2
past5 %>% filter(`Primary Type` == "HOMICIDE", Year == 2020) %>%
  group_by(`Community Area`) %>%
  summarize(total_count = n()) %>%
  mutate(`Community Area` = factor(`Community Area`)) %>%
  right_join(chi_areas, by=c("Community Area" = "area_num_1")) %>%
  ggplot(aes(geometry = geometry, fill = total_count)) + 
  geom_sf() +
  scale_fill_viridis_c(name="Total Cases", alpha = .5) +
  geom_sf_text(aes(label = `Community Area`), size = 2) +
  theme_minimal() +
  labs(x="Latitude", y="Longitude", 
       title = "Homicide by Community Areas in Chicago",
       subtitle = "Total Reported Case in 2020",
       caption="Source: Chicago Police Department")
