library(tidyverse)

# Load file
attendance_raw <- read_csv("mlb_games.csv")

#### Data prep ####
# Convert to date
attendance_raw$gameDate <- as.POSIXct(paste(attendance_raw$game_date, attendance_raw$start_time, sep = " "),
                                      format = "%Y-%m-%d %H:%M:%S")

## Remove home games at non-home stadiums (i.e. Astros playing in Tampa)
# Find the games
temp_games <- attendance_raw %>%
  group_by(venue.id, teams.home.team.name) %>%
  summarise(Count = n()) %>%
  filter(Count < 10) 

# Now remove the those games
att_red <- attendance_raw[!paste0(attendance_raw$venue.id, attendance_raw$teams.home.team.name) %in% 
                            paste0(temp_games$venue.id, temp_games$teams.home.team.name),]


### Get specifics
# add day of week
att_red$weekday <- weekdays(att_red$gameDate)
att_red$weekday <- factor(att_red$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Code Florida/Miami Marlins as the same team
att_red[att_red$teams.home.team.name == "Florida Marlins", "teams.home.team.name"] <- "Miami Marlins"
att_red[att_red$teams.away.team.name == "Florida Marlins", "teams.away.team.name"] <- "Miami Marlins"

# Get winning percentage by year
temp <- list()
years <- 2008:2019
counter <- 1
for(x in 1:12){
  for(league in c("AL", "NL")){
    for(division in c("East", "West", "Central")){
      temp[[counter]] <- baseballr::standings_on_date_bref(paste0(years[x], "-11-10"),
                                                           paste(league, division, sep = " ")) %>%
        data.table::rbindlist() %>%
        mutate(Year = years[x])
      
      # Increase the counter
      counter <- counter + 1
    }
  }
}

# Code lower case "clear" to "Clear" to match others
att_red[att_red$other_weather == "clear", "other_weather"] <- "Clear"

# Combine each years data
records <- temp %>% data.table::rbindlist() %>%
  select(Team = Tm, season = Year, Win.pct = `W-L%`)

# Get team abbreviations from baseball reference to join with attendance data
teams <- baseballr::teams_lu_table

# Code Marlins and correct others to line up baseball reference Team IDs
records$Team[records$Team == "FLA"] <- "MIA"
records$Team[records$Team == "CHW"] <- "CWS"
records$Team[records$Team == "KCR"] <- "KC"
records$Team[records$Team == "SDP"] <- "SD"
records$Team[records$Team == "SFG"] <- "SF"
records$Team[records$Team == "TBR"] <- "TB"
records$Team[records$Team == "WSN"] <- "WSH"

# Join abbreviations with records
records2 <- left_join(records, teams[teams$active == T  & teams$sport.name == "Major League Baseball",c("name", "abbreviation")],
                      by = c("Team" = "abbreviation"))

# Get seasons ahead for joining purposes
records2$season.behind <- records2$season + 1
records2$season.two.behind <- records2$season + 2


#### Add home team records from two years back ####
# join
att_red2 <- left_join(att_red, records2[,c("season.two.behind", "name", "Win.pct")],
                      by = c("season" = "season.two.behind", "teams.home.team.name" = "name"))
# Rename column
colnames(att_red2)[colnames(att_red2) == "Win.pct"] <- "team.home.two.behind.win.pct"

## Add away team records from two years back
# join
att_red2 <- left_join(att_red2, records2[,c("season.two.behind", "name", "Win.pct")],
                      by = c("season" = "season.two.behind", "teams.away.team.name" = "name"))
# Rename column
colnames(att_red2)[colnames(att_red2) == "Win.pct"] <- "team.away.two.behind.win.pct"

## Add home team records from one year back
# join
att_red2 <- left_join(att_red2, records2[,c("season.behind", "name", "Win.pct")],
                      by = c("season" = "season.behind", "teams.home.team.name" = "name"))
# Rename column
colnames(att_red2)[colnames(att_red2) == "Win.pct"] <- "team.home.one.behind.win.pct"

## Add away team records from two years back
# join
att_red2 <- left_join(att_red2, records2[,c("season.behind", "name", "Win.pct")],
                      by = c("season" = "season.behind", "teams.away.team.name" = "name"))
# Rename column
colnames(att_red2)[colnames(att_red2) == "Win.pct"] <- "team.away.one.behind.win.pct"

## Add home team records from that year
# join
att_red2 <- left_join(att_red2, records2[,c("season", "name", "Win.pct")],
                      by = c("season" = "season", "teams.home.team.name" = "name"))
# Rename column
colnames(att_red2)[colnames(att_red2) == "Win.pct"] <- "team.home.win.pct"

## Add away team records from two years back
# join
att_red2 <- left_join(att_red2, records2[,c("season", "name", "Win.pct")],
                      by = c("season" = "season", "teams.away.team.name" = "name"))
# Rename column
colnames(att_red2)[colnames(att_red2) == "Win.pct"] <- "team.away.win.pct"
#####

# Save final data for modeling
write_csv(att_red2, "attendance_modeling.csv")
