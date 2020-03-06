library(tidyverse)
library(prophet)
library(lubridate)

# load file
attendance_raw <- read_csv("mlb_games.csv")

# Convert to date time
attendance_raw$gameDate <- as.POSIXct(paste(attendance_raw$game_date, attendance_raw$start_time, sep = " "),
                   format = "%Y-%m-%d %H:%M:%S")

## Remove home games at non-home stadiums (i.e. Astros playing in Tampa)
# Find the games
temp_games <- attendance_raw %>%
  group_by(venue.id, teams.home.team.name) %>%
  summarise(Count = n()) %>%
  filter(Count < 10) 

temp2 <- attendance_raw[paste0(attendance_raw$venue.id, attendance_raw$teams.home.team.name) %in% 
                            paste0(temp_games$venue.id, temp_games$teams.home.team.name),]
# Now remove the those games
att_red <- attendance_raw[!paste0(attendance_raw$venue.id, attendance_raw$teams.home.team.name) %in% 
  paste0(temp_games$venue.id, temp_games$teams.home.team.name),]

# Plot season-by-season average attendance for the Dodgers
att_red %>%
  filter(teams.home.team.name == "Los Angeles Dodgers") %>%
  group_by(season) %>%
  summarise(Avg.att = mean(attendance)) %>%
  mutate(att.display = formatC(Avg.att,
                               format = "f", big.mark = ",", digits = 0)) %>%
  ggplot(aes(x = season, y = Avg.att))+
  geom_col(fill = "dodgerblue4")+
  scale_y_continuous(name = "Average Attendance", 
                     breaks = c(10000, 20000, 30000, 40000, 50000),
                     labels = c("10,000", "20,000", "30,000", "40,000", "50,000"))+
  scale_x_continuous(name = "Season",
                    breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
                    labels = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"))+
  geom_text(aes(label = att.display, vjust = 1.5), color = "white")+
  theme_minimal()+
  labs(title = "Los Angeles Dodgers Average Attendance by Season")+
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(size = 12, vjust = 10),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


# Can see where attendenace really started to pick up in 2013 with McCourt selling the team in 2012
# Focus on 2013 onward

### Get specifics
# add day of week
att_red$weekday <- weekdays(att_red$gameDate)
att_red$weekday <- factor(att_red$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Plot average attendence 
att_week <- att_red %>%
  filter(teams.home.team.name == "Los Angeles Dodgers", season >= 2013) %>%
  mutate(avg = mean(attendance)) %>%
  group_by(weekday) %>%
  summarise(Avg.att = mean(attendance), avg = max(avg)) %>%
  mutate(avg.diff = Avg.att - avg,
         att.display = formatC(Avg.att,
                               format = "f", big.mark = ",", digits = 0),
         dif.display = formatC(avg.diff,
                               format = "f", big.mark = ",", digits = 0))

# Plot average attendence by weekday
att_week %>%
  ggplot(aes(x = weekday, y = Avg.att))+
  geom_col(fill = "dodgerblue4")+
  scale_y_continuous(name = "Average Attendance", 
                     breaks = c(10000, 20000, 30000, 40000, 50000),
                     labels = c("10,000", "20,000", "30,000", "40,000", "50,000"))+
  geom_text(aes(label = att.display, vjust = 1.5), color = "white")+
  theme_minimal()+
  labs(title = "Los Angeles Dodgers Average Attendance by Day of Week\n(2013-2019)",
       x = "Day of Week")+
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(size = 11, vjust = 10),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Plot difference
att_week %>%
  ggplot(aes(x = weekday, y = avg.diff))+
  geom_col(fill = "dodgerblue4")+
  scale_y_continuous(name = "Difference in Attendance", 
                     breaks = c(-2000, 0, 2000),
                     labels = c("-2,000", "0", "2,000"))+
  geom_text(aes(label = dif.display), color = "red", vjust = -.5)+
  theme_minimal()+
  labs(title = "Los Angeles Dodgers Difference in Attendance by Day of Week\n(2013-2019)")+
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(size = 11, vjust = 0),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank())


# Plot average attendance by temperature
att_red %>%
  filter(teams.home.team.name == "Los Angeles Dodgers", season >= 2013) %>%
  group_by(temperature) %>%
  summarise(avg.att = mean(attendance), count = n()) %>%
  ggplot()+
  geom_smooth(aes(x = temperature, y = avg.att, weight = count), color = "black", se = T)+
  geom_point(aes(x = temperature, y = avg.att, size = count), color = "dodgerblue4")+
  theme_minimal()+
  scale_y_continuous(name = "Average Attendance", 
                     breaks = c(30000, 35000, 40000, 45000, 50000, 55000),
                     labels = c("30,000", "35,000", "40,000", "45,000", "50,000", "55,000"),
                     limits = c(35000, 55000))+
  scale_x_continuous(name = "Temperature (°F)", 
                     breaks = c(60, 70, 80, 90, 100),
                     labels = c("60", "70", "80", "90", "100"))+
  theme(plot.title = element_text(hjust = .5))+
  labs(title = "Los Angeles Dodgers Average Attendance by Temperature\n(2013-2019)", size = "Count")



# Plot attendance by date
att_red %>%
  filter(teams.home.team.name == "Los Angeles Dodgers", season >= 2013) %>%
  mutate(samedate = as.Date(paste("2019", month(gameDate), day(gameDate), sep = "-"), format = "%Y-%m-%d")) %>%
  group_by(samedate) %>%
  summarise(avg.att = mean(attendance), count = n()) %>%
  ggplot(aes(x = samedate, y = avg.att, weight = count))+
  geom_point(color = "dodgerblue4")+
  geom_smooth(color = "black")+
  theme_minimal()+
  scale_y_continuous(name = "Average Attendance", 
                     breaks = c(30000, 35000, 40000, 45000, 50000, 55000),
                     labels = c("30,000", "35,000", "40,000", "45,000", "50,000", "55,000"),
                     limits = c(35000, 55000))+
  theme(plot.title = element_text(hjust = .5))+
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%B")+
  labs(title = "Los Angeles Dodgers Attendance by Date\n(2013-2019)")

# Plot attendance by date with temperature
att_red %>%
  filter(teams.home.team.name == "Los Angeles Dodgers", season >= 2013) %>%
  mutate(samedate = as.Date(paste("2019", month(gameDate), day(gameDate), sep = "-"), format = "%Y-%m-%d")) %>%
  group_by(samedate) %>%
  summarise(avg.att = mean(attendance), avg.temp = mean(temperature)) %>%
  ggplot()+
  geom_point(aes(x = samedate, y = avg.att, color = avg.temp), size = 3)+
  theme_minimal()+
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%B")+
  scale_y_continuous(name = "Average Attendance",
                     breaks = c(30000, 35000, 40000, 45000, 50000, 55000),
                     labels = c("30,000", "35,000", "40,000", "45,000", "50,000", "55,000"),
                     limits = c(35000, 55000))+
  theme(plot.title = element_text(hjust = .5), legend.title = element_text(hjust = .5))+
  labs(title = "Los Angeles Dodgers Attendance by Date\n(2013-2019)")+
  scale_color_gradient(name = "Average\nTemperature", 
                       low = "dodgerblue1", high = "orangered1",
                       breaks = c(65, 75, 85),
                       labels = c("65°F", "75°F", "85°F"), 
                       limits = c(60, 90))
    

# See what attendence is on opening day and all other days
att_red %>%
  filter(teams.home.team.name == "Los Angeles Dodgers", season >= 2013) %>%
  group_by(opener) %>%
  summarise(avg.att = mean(attendance))

### See influence of road team
# Code Florida/Miami Marlins as one team
att_red[att_red$teams.home.team.name == "Florida Marlins", "teams.home.team.name"] <- "Miami Marlins"
att_red[att_red$teams.away.team.name == "Florida Marlins", "teams.away.team.name"] <- "Miami Marlins"

# Show affect of road team on average attendance 
att_red %>%
  group_by(teams.home.team.name) %>%
  mutate(home.avg.att = mean(attendance)) %>%
  ungroup() %>%
  mutate(game.att.diff = (attendance - home.avg.att)/home.avg.att) %>%
  group_by(teams.away.team.name) %>%
  summarise(att.diff = mean(game.att.diff)) %>%
  ggplot(aes(x = reorder(teams.away.team.name, att.diff), y = att.diff))+
  geom_col(fill = "dodgerblue4")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5), 
        panel.grid.minor.x = element_blank())+
  #geom_text(aes(label = teams.away.team.name))+
  scale_y_continuous(name = "Change in Attendence", 
                     breaks = c(-.05, 0, .05, .1, .15, .2),
                     labels = c("-5%", "0%", "5%", "10%", "15%", "20%"),
                     limits = c(-.07, .21))+
  theme(plot.title = element_text(hjust = .5))+
  labs(x = "Away Team", title = "Major League Baseball Change in Attendance by Away Team\n(2010-2019)")+
  coord_flip()

### Examine the Braves winning percentage and attendance as the away team
# Get change in attendance when the braves are the away team
braves_att <- att_red %>%
  group_by(teams.home.team.name) %>%
  mutate(home.avg.att = mean(attendance)) %>%
  ungroup() %>%
  filter(teams.away.team.name == "Atlanta Braves") %>%
  mutate(game.att.diff = (attendance - home.avg.att)/home.avg.att) %>%
  group_by(season) %>%
  summarise(att.diff = mean(game.att.diff))

## Get Braves winning percentage each year
# Extract the data from baseball reference
temp <- list()
years <- 2010:2019
for(x in 1:10){
  temp[[x]] <- baseballr::standings_on_date_bref(paste0(years[x], "-11-10"),
                                               "NL East") %>%
    data.table::rbindlist() %>%
    mutate(Year = years[x])
}

# Combine each years data
braves_records <- temp %>% data.table::rbindlist() %>%
  filter(Tm == "ATL") %>%
  select(season = Year, Win.pct = `W-L%`)

# Combine attendence data with record
braves_dat <- left_join(braves_att, braves_records, by = "season")

# Plot the data
braves_dat %>%
  ggplot()+
  geom_col(aes(x = season, y = att.diff), fill = "#13274F")+
  geom_line(aes(x = season, y = Win.pct-.5), color = "#CE1141", size = 1)+
  theme_minimal()+
  scale_x_continuous(name = "Season",breaks = 2010:2019)+
  scale_y_continuous("Difference in Attendance",
                     breaks = c(-.1, -.05, 0, .05, .1),
                     labels = c("-10%", "-5%", "0%", "5%", "10%"),
                     sec.axis = sec_axis(~ . + .5, name = "Winning Percentage",
                                         breaks = c(.4, .45, .5, .55, .60),
                                         labels = c(".400", ".450", ".500", ".550", ".600")))+
  theme(plot.title = element_text(hjust = .5), 
        panel.grid.minor.x = element_blank())+
  labs(title = "Atlanta Braves Difference in Attendence as the Road Team and Winniner Percentage\n(2010-2019)")
  
  
  
