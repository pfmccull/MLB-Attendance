library(tidyverse)

# Initialize list 
games <- list()

# Initialize count (this skips dates without a game)
i <- 1

# Get the games for each day
for(yy in 2010:2019){
  for(mm in 3:10){
    for(dd in 1:31){
      games[[i]] <- try(baseballr::get_game_pks_mlb(paste(yy, mm, dd, sep = "-")), silent = T)
      if(length(games[[i]]) > 1){
        i <- i + 1
      }
    }
  }
}

# Remove the last observation (there are no games)
games_red <- games[1:(length(games)-1)]

# Bind the data together
data <- data.table::rbindlist(games_red, fill = T)

# Limit to just regular season games
data_red <- data[data$seriesDescription == "Regular Season", ]

### Now get more specific game data
game_data <- list()

# Loop through and get the specific data for each game
for(x in 1:length(data_red$game_pk)){
  game_data[[x]] <- try(baseballr::get_game_info_mlb(data_red$game_pk[x]), silent = T)
}


## Error checking
# Get the length of each element of the list
game_data_length <- do.call(rbind, lapply(game_data, length)) 
# Remove those with length 1 (errors)
game_data <- game_data[game_data_length != 1]

# Bind the data together
game_data_df <- data.table::rbindlist(game_data, fill = T)

# Combine the data on each game
games_complete <- left_join(data_red, game_data_df, by = "game_pk")

# Remove superflous columns
attendance_red <- games_complete %>%
  select(-gameType, -calendarEventID, -seasonDisplay, -ifNecessary,
         -link, -publicFacing, -scheduledInnings, -inningBreakLength, 
         -ifNecessaryDescription, -status.abstractGameState, -status.abstractGameCode,
         -teams.away.splitSquad, -teams.away.team.link, -teams.home.splitSquad, -teams.home.team.link,
         -venue.link, -content.link, -game_id, -official_scorer, 
         -date, -home_league_id, -home_sport_code,-gameday_sw, -game_type, -gamedayType, -venue_name, -venue_id)

# Find duplicates
dups <- attendance_red %>%
  group_by(game_pk) %>%
  summarise(Count = n()) %>%
  filter(Count >= 2)
### Every game_pk has the same attendance count, we want the first one

# Get duplicate games
att_dup <- attendance_red %>%
  filter(game_pk %in% dups$game_pk) %>%
  unique()

## It appears that the duplicate games are almost always postponed. The dates of the games are 
# listed when it was completed, but it's hard to know if this influences the attendence figure, 
# so remove them all together.

# Remove duplicate games from the df
attendance_red <- attendance_red %>%
  filter(!game_pk %in% dups$game_pk)

# Remove game with no attendence listed and tie-breakers
attendance_red <- attendance_red %>%
  filter(!is.na(attendance), tiebreaker == "N")

# Let's get the venue count for special events
specials <- attendance_red %>%
  group_by(venue.name) %>%
  summarise(Count = n()) %>%
  filter(Count < 70)

# Remove special event games
attendance_red <- attendance_red %>%
  filter(!venue.name %in% specials$venue.name)

## Code openers
# Set NA descriptions to blank
attendance_red$description[is.na(attendance_red$description)] <- ""
# Code openers
attendance_red$opener <- str_detect(attendance_red$description, "opener")


# Save the data
write_csv(attendance_red, "mlb_games.csv")


