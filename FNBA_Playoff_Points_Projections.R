library(stringr)
library(dplyr)
library(sqldf)
library(combinat)
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("count", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("summarise", "dplyr")

# Every team's schedule during the 4 week playoff
Blazers <- c(	1,	0,	1,	0,	0,	0,	1,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	0,	1,	0,	1,	0,	1,	1,	0,	1)
Bucks <- c(	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	1)
Bulls <- c(	1,	0,	0,	1,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	0,	1,	0,	1,	0,	1,	1,	0,	0,	1,	0,	1)
Cavaliers <- c(	0,	1,	0,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	0,	1,	0,	1,	0,	1)
Celtics <- c(	1,	0,	1,	0,	1,	1,	0,	0,	1,	0,	0,	1,	0,	1,	1,	0,	1,	0,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1)
Clippers <- c(	0,	1,	1,	0,	1,	0,	1,	0,	1,	0,	0,	1,	0,	1,	0,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	1,	0)
Grizzlies <- c(	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	1,	0,	0,	0,	1,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0)
Hawks <- c(	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	0,	0,	1,	1,	0,	1,	0,	1,	0,	0,	1,	0,	1,	1,	0,	0,	1,	1)
Heat <- c(	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0)
Hornets <- c(	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	1)
Jazz <- c(	1,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	1,	0,	0,	1,	0,	1,	0,	1,	0,	1)
Kings <- c(	1,	0,	0,	1,	1,	0,	0,	1,	0,	1,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1)
Knicks <- c(	1,	0,	1,	0,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	0,	1,	1,	0,	1,	0,	1,	0,	1,	1,	0,	0,	1,	1)
Lakers <- c(	1,	0,	0,	1,	1,	0,	1,	1,	0,	1,	1,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	0,	1,	1,	0,	1)
Magic <- c(	1,	0,	0,	1,	1,	0,	1,	0,	0,	1,	0,	1,	0,	0,	1,	1,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	0,	0)
Mavericks <- c(	1,	0,	1,	0,	1,	0,	1,	0,	0,	1,	0,	1,	0,	0,	1,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	1,	0)
Nets <- c(	1,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	0,	1,	0,	0,	1)
Nuggets <- c(	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	0,	0,	1,	1,	0,	1,	0,	1)
Pacers <- c(	1,	1,	0,	0,	1,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1)
Pelicans <- c(	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	1,	0,	0,	0,	1,	0,	1,	0,	0,	1,	0,	1,	0,	1)
Pistons <- c(	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	0,	1,	0,	1,	0,	0,	1,	0,	1,	1,	0)
Raptors <- c(	1,	0,	1,	0,	1,	0,	1,	1,	0,	0,	1,	0,	0,	1,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	1,	0,	1)
Rockets <- c(	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	0,	1,	1,	0,	1,	0,	1,	0,	1)
Sixers <- c(	1,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	0,	1,	1,	0,	1,	0,	1,	0,	1,	0)
Spurs <- c(	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	1,	0,	1,	1,	0,	1,	0,	1)
Suns <- c(	1,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	0,	1,	0,	1)
Thunder <- c(	1,	0,	1,	0,	0,	1,	1,	0,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1)
Timberwolves <- c(	0,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	0,	1,	0,	0,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0)
Warriors <- c(	1,	0,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	0,	1,	0,	0,	1,	0,	1,	0,	1,	0,	1,	1,	0,	1)
Wizards <- c(	1,	1,	0,	1,	0,	1,	0,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	1,	0,	1,	0,	1,	0,	1,	1,	0,	0,	1)


# List of potential players to add
# PlayerName <- Team
# PlayerName_Add <- c('PlayerName',ProjPPG,Positions)
Kai <- Mavericks
Kai_Add <- c('Kai',20,'C')
Rollins <- Bucks
Rollins_Add <- c('Rollins',16,'PG')
Reese <- Sixers
Reese_Add <- c('Reese',13,'SF/PF/C')
Drummond <- Sixers
Drummond_Add <- c('Drummond',20,'C')
Gillespie <- Suns
Gillespie_Add <- c('Gillespie',14,'PG')
Test <- Raptors
Test_Add <- c('Test',15,'SG')

# Set what day you're at if you're running after the playoffs have started
# Set to 1 if the playoffs haven't started yet
today <- 25

# Select players to add/subtract
players_subtracted <- c()
players_added <- rbind()

# Get PlayerName, ProjPPG, and Positions for players added
names_added <- c(players_added[,1])
points_added <- c(players_added[,2])
positions_added <- c(players_added[,3])

# List of players currently on your team
Tobias <- Pistons
Nembhard <- Pacers
Keegan <- Kings
Jovic <- Heat
Ellis <- Kings
Watson <- Nuggets
Green <- Bucks
Giannis <- Bucks
Chet <- Thunder
Quickley <- Raptors
George <- Wizards
Vit <- Hawks
Jeffries <- Hornets
Walker <- Blazers
Gary <- Warriors

# List of unique teams on your team
unique_teams <- c('Pistons','Pacers','Kings','Heat','Nuggets','Bucks','Thunder','Raptors',
                  'Wizards','Hawks','Hornets','Blazers','Warriors')

# Set your whole teams' schedule into a data frame along with any players added
if (length(names_added) == 0) {
  schedule <- data.frame(Tobias,Nembhard,Keegan,Jovic,Ellis,Watson,Green,Giannis,
                         Chet,Quickley,George,Vit,Jeffries,Walker,Gary)
} else {
  schedule <- data.frame(Tobias,Nembhard,Keegan,Jovic,Ellis,Watson,Green,Giannis,
                         Chet,Quickley,George,Vit,Jeffries,Walker,Gary,mget(names_added))
}


# List of players on your team
players <- c("Tobias","Nembhard","Keegan","Jovic","Ellis",
             "Watson","Green","Giannis","Chet","Quickley",
             "George",'Vit','Jeffries','Walker','Gary',names_added)

# Projected points per game for each player on your team. ***Same order as above***
points <- c(30,25,28,25,21,15,15,55,38,36,28,18,15,13,18,points_added)

# Positions for each player on your team. ***Same order as above***
positions <- c("SF/PF","PG/SG","SF/PF","PF","SG/SF","SG/SF/PF","SG",
               "PF/C","PF/C","PG/SG","PG/SG/SF",'PG/SG/SF','SG','SF/PF','PG/SG',
               positions_added)

# Create dictionaries for players and their corresponding ppg projections and positions
players_points <- setNames(points,players)
players_positions <- setNames(positions,players)

# Remove any subtracted players from the list of player names
if (length(players_subtracted) != 0) {
  for (i in players_subtracted) {
    players <- players[players != i]
  }
}

# Create 2 empty matrix with 28 rows (number of days in the playoffs) and:
# Columns - Round, Day, Starting Lineup, Reserves

# Will become final schedule with player names
df_pyrs <- matrix(ncol=17,nrow=28)
colnames(df_pyrs) <- c("Round","Day","PG","SG","SF","PF","C","Flx1","Flx2","Flx3",
                       "Res1","Res2","Res3","Res4","Res5","Res6","Res7")
# Will become final schedule with PPG
df_pts <- matrix(0,ncol=17,nrow=28)
colnames(df_pts) <- c("Round","Day","PG","SG","SF","PF","C","Flx1","Flx2","Flx3",
                      "Res1","Res2","Res3","Res4","Res5","Res6","Res7")

# List of non-flex starting lineup spots
starting_5_pos <- c("PG", "SG", "SF", "PF", "C")
# Get all permutations of the order of the above positions
# This is used later to ensure that the optimal starting lineup (max total proj pts) is chosen
all_permutations <- permn(starting_5_pos)

# Begin lineup optimization

# i for 4 rounds
for (i in 1:4) {
  # j for 7 days/round
  for (j in 1:7) {
    # Running total of days from the start
    rnum <- 7*(i-1) + j
    # Temp datasets to be reset for each day 
    df_pyrs_tmp_1 <- matrix(ncol=17,nrow=1)
    colnames(df_pyrs_tmp_1) <- c("Round","Day","PG","SG","SF","PF","C","Flx1","Flx2","Flx3",
                                 "Res1","Res2","Res3","Res4","Res5","Res6","Res7")
    df_pts_tmp_1  <- matrix(0,ncol=17,nrow=1)
    colnames(df_pyrs_tmp_1) <- c("Round","Day","PG","SG","SF","PF","C","Flx1","Flx2","Flx3",
                                 "Res1","Res2","Res3","Res4","Res5","Res6","Res7")
    # Makes sure every different lineup setting order is tried
    for (p in 1:length(all_permutations)) {
      # Temp datasets to be reset after each permutation
      df_pyrs_tmp_2 <- matrix(ncol=17,nrow=1)
      colnames(df_pyrs_tmp_2) <- c("Round","Day","PG","SG","SF","PF","C","Flx1","Flx2","Flx3",
                                   "Res1","Res2","Res3","Res4","Res5","Res6","Res7")
      df_pts_tmp_2  <- matrix(0,ncol=17,nrow=1)
      colnames(df_pts_tmp_2) <- c("Round","Day","PG","SG","SF","PF","C","Flx1","Flx2","Flx3",
                                  "Res1","Res2","Res3","Res4","Res5","Res6","Res7")
      df_pyrs_tmp_2[1,"Round"] <- i
      df_pyrs_tmp_2[1,"Day"] <- j
      df_pts_tmp_2[1,"Round"] <- i
      df_pts_tmp_2[1,"Day"] <- j
      pyr <- c()
      pt <- 0
      # Create temp list of players to remove players after they're set in the lineup
      players_1 <- players
      for (k in 1:length(all_permutations[[p]])) {
        for (l in players_1) {
          # For each position, get the max pts for all players that are eligible at that position and have a game on the current day
          if (str_like(players_positions[l],paste0("%",all_permutations[[p]][k],"%")) & schedule[l][rnum,1] == 1) {
            pt_tmp = as.numeric(players_points[l])
            if (pt_tmp > pt) {
              pyr <- l
              pt <- pt_tmp
            }
          }
        }
        if (pt > 0) {
          # Set the final player chosen and their PPG to the spot
          df_pyrs_tmp_2[1,all_permutations[[p]][k]] <- pyr
          df_pts_tmp_2[1,all_permutations[[p]][k]] <- pt
          # Remove the chosen player from the tmp player list
          players_1 <- players_1[players_1 != pyr]
          pyr <- c()
          pt <- 0
        }
      }
      # Pick the top 3 PPG players remaining for the flex spots and put the rest in reserve
      for (k in c("Flx1","Flx2","Flx3","Res1","Res2","Res3","Res4","Res5","Res6","Res7")) {
        for (l in players_1) {
          if (schedule[l][rnum,1] == 1) {
            pt_tmp = as.numeric(players_points[l])
            if (pt_tmp > pt) {
              pyr <- l
              pt <- pt_tmp
            }
          }
        }
        if (pt > 0) {
          df_pyrs_tmp_2[1,k] <- pyr
          df_pts_tmp_2[1,k] <- pt
          players_1 <- players_1[players_1 != pyr]
          pyr <- c()
          pt <- 0
        }
      }
      # Sum the total PPG for the starting lineup and keep replacing until you get the highest possible
      if (sum(df_pts_tmp_2[1,3:10]) > sum(df_pts_tmp_1[1,3:10])) {
        df_pts_tmp_1 <- df_pts_tmp_2
        df_pyrs_tmp_1 <- df_pyrs_tmp_2
      }
    }
    # After trying all the permutation, add the chosen lineup for the day to the final lineup and move to the next day
    df_pts[rnum,] <- df_pts_tmp_1
    df_pyrs[rnum,] <- df_pyrs_tmp_1
  }
}

df_pts <- as.data.frame(df_pts)

# Get the totals for each day
day_totals_tmp <- sqldf("select Round, Day, sum(PG+SG+SF+PF+C+Flx1+Flx2+Flx3) as Lineup_Pts,
                      (count(NULLIF(PG,0))+count(NULLIF(SG,0))+count(NULLIF(SF,0))+
                      count(NULLIF(PF,0))+count(NULLIF(C,0))+count(NULLIF(Flx1,0))+
                      count(NULLIF(Flx2,0))+count(NULLIF(Flx3,0))) as Lineup_Cnt,
                      sum(Res1+Res2+Res3+Res4+Res5+Res6+Res7) as Bench_Pts,
                      (count(NULLIF(Res1,0))+count(NULLIF(Res2,0))+count(NULLIF(Res3,0))+
                      count(NULLIF(Res4,0))+count(NULLIF(Res5,0))+count(NULLIF(Res6,0))+
                      count(NULLIF(Res7,0))) as Bench_Cnt
                    from df_pts
                    group by Round, Day
                    order by Round, Day ")
day_totals_tmp <- cbind(day_totals_tmp,'Row_Number'=seq(1,28))
day_totals <- day_totals_tmp[today:28,]

# Get the totals for each round
round_totals <- sqldf("select Round, sum(Lineup_pts) as Lineup_Pts,
                        sum(Lineup_Cnt) as Lineup_Cnt,
                        sum(Bench_pts) as Bench_Pts,
                        sum(Bench_Cnt) as Bench_Cnt
                      from day_totals
                      group by Round
                      order by Round")

# Get the difference compared to your current team
# Bench pts/cnt is used to see how many pts are wasted on the bench
current_lineup_pts <- c(1319,1299,1335,1356)
current_lineup_cnt <- c(91,59,13,134)
current_bench_cnt <-  c(6,4,1,9)

# If today is in the middle of a playoff week, the current points will need to be rerun with no players
# added/subtracted and the above values will need updated (if you care about the comparison)
# Below ensures that the code still runs if today > 7
if (today > 7 & today <= 14) {
  current_lineup_pts <- current_lineup_pts[2:4]
  current_lineup_cnt <- current_lineup_cnt[2:4]
  current_bench_cnt <- current_bench_cnt[2:4]
} else if (today > 14 & today <= 21) {
  current_lineup_pts <- current_lineup_pts[3:4]
  current_lineup_cnt <- current_lineup_cnt[3:4]
  current_bench_cnt <- current_bench_cnt[3:4]
} else if (today > 21) {
  current_lineup_pts <- current_lineup_pts[4]
  current_lineup_cnt <- current_lineup_cnt[4]
  current_bench_cnt <- current_bench_cnt[4]
}
  
  

round_totals["Lineup_Pts_Diff"] <- round_totals["Lineup_Pts"] - current_lineup_pts
round_totals["Bench_Pts_Diff"] <- round_totals["Bench_Pts"] - current_lineup_cnt
round_totals["Bench_Cnt_Diff"] <- round_totals["Bench_Cnt"] - current_bench_cnt

# Get the totals for the playoffs as a whole
totals <- sqldf("select sum(Lineup_pts) as Lineup_pts, sum(Lineup_Cnt) as Lineup_Cnt,
                sum(Bench_pts) as Bench_pts, sum(Bench_Cnt) as Bench_Cnt,
                sum(Lineup_Pts_Diff) as Lineup_Pts_Diff,
                sum(Bench_Pts_Diff) as Bench_Pts_Diff,
                sum(Bench_Cnt_Diff) as Bench_Cnt_Diff
                from round_totals")


## This section is for analysis on the best teams to add, subtract, or swap for each other  

team_names <- c('Hawks',	'Celtics',	'Nets',	'Hornets',	'Bulls',	'Cavaliers',	'Mavericks',	'Nuggets',	'Pistons',	'Warriors',	'Rockets',	'Pacers',	'Clippers',	'Lakers',	'Grizzlies',	'Heat',	'Bucks',	'Timberwolves',	'Pelicans',	'Knicks',	'Thunder',	'Magic',	'Sixers',	'Suns',	'Blazers',	'Kings',	'Spurs',	'Raptors',	'Jazz',	'Wizards')
teams_tmp <- data.frame(rbind(Hawks,	Celtics,	Nets,	Hornets,	Bulls,	Cavaliers,	Mavericks,	Nuggets,	Pistons,	Warriors,	Rockets,	Pacers,	Clippers,	Lakers,	Grizzlies,	Heat,	Bucks,	Timberwolves,	Pelicans,	Knicks,	Thunder,	Magic,	Sixers,	Suns,	Blazers,	Kings,	Spurs,	Raptors,	Jazz,	Wizards))
teams <- teams_tmp[,today:28]

# Counts of days each team plays when you have a starting lineup spot open
open_days <- which(day_totals$Lineup_Cnt<=7 & day_totals$Bench_Cnt == 0)
teams_open_days <- teams[,open_days]
open_day_sums <- rowSums(teams_open_days[,1:length(teams_open_days)])
open_day_sums_sorted <- open_day_sums %>%
  sort(decreasing = TRUE) %>%
  as.matrix()
colnames(open_day_sums_sorted) <- 'Open_Days'

# For each team in your lineup, count of days they play where your lineup is full with at least 1 player on the bench
full_days <- which(day_totals$Lineup_Cnt == 8 & day_totals["Bench_Cnt"]>=1 & (7*(day_totals$Round-1)+day_totals$Day) >= today)
teams_full_days <- teams[unique_teams,full_days,drop=FALSE]
full_day_sums <- rowSums(teams_full_days[,1:length(teams_full_days)])
full_day_sums_sorted <- full_day_sums %>%
  sort(decreasing = TRUE) %>%
  as.matrix()
colnames(full_day_sums_sorted) <- 'Full_Days'


# Total games each team plays
total_games <- rowSums(teams)
total_games_sorted <- total_games %>%
  sort(decreasing = TRUE) %>%
  as.matrix()
colnames(total_games_sorted) <- 'Total_Games'


# Total non-full days for each team in your lineup (i.e. losing the player would result in this many starts lost)
non_full_days <- total_games[unique_teams] - full_day_sums
non_full_days_sorted <- non_full_days %>%
  sort(decreasing = TRUE) %>%
  as.matrix()
colnames(non_full_days_sorted) <- 'Non_Full_Days'





# Function to see total starts lost/gained (regardless of ppg) when you swap one team for another
# Also shows where the max number of starts up to a certain point (and when that certain point is)
# Team1 is the player you're swapping in
# Team2 is the player currently on your roster that you're swapping out,
team_compare <- function(team1,team2) {
  team1 <- data.frame(team1)
  team2 <- data.frame(team2)
  compare <- cbind(day_totals['Lineup_Cnt'],day_totals['Bench_Cnt'],team1[today:28,],team2[today:28,])
  num <- 0
  max <- -100
  # Iterate over each day and see if swapping one team with the other would result in an addition
  # to the starting lineup, a subtraction, or no change
  for (i in 1:(29-today)) {
    # If they both play or don't play on a day, then no change
    if (compare[i,3] != compare[i,4]) {
      # If Team1 plays and there's an open lineup spot, then they add to the lineup count
      if (compare[i,'Lineup_Cnt'] <= 7 & compare[i,3] == 1) {
        num = num + 1
      # If the player on your team is playing and you don't have anyone on your bench to replace them,
      # then they subtract from the lineup count
      } else if (compare[i,'Bench_Cnt'] == 0 & compare[i,4] == 1) {
        num = num - 1
      }
    }
    # Below is used to determine at what point the number of starts added is at it's maximum
    # This can be used if you want to add a player with the plan of replacing them at a certain point after
    if (num > max) {
      max <- num
      day_1 <- today + i - 1
    }
    if (num >= max) {
      day_2 <- today + i - 1
    }
  }
  return(cbind(num,max,day_1,day_2))
}


df_compare_teams <- data_frame()
num <- 1
for (team1 in team_names) {
  for (team2 in unique_teams) {
    if (team1 != team2) {
      df_compare_teams[num,'Team_1'] <- team1
      df_compare_teams[num,'Team_2'] <- team2
      # Total number of starts gained/lost
      df_compare_teams[num,'Total_Value'] <- team_compare(mget(team1),mget(team2))[1]
      # Maximum number of starts gained at any point
      df_compare_teams[num,'Max_Value'] <- team_compare(mget(team1),mget(team2))[2]
      # The 2 dates between which the Max_Value occurs
      df_compare_teams[num,'Max_Day_init'] <- team_compare(mget(team1),mget(team2))[3]
      df_compare_teams[num,'Max_Day_final'] <- team_compare(mget(team1),mget(team2))[4]
      num <- num + 1
    }
  }
}
df_compare_teams <- df_compare_teams[order(df_compare_teams$Max_Value, df_compare_teams$Total_Value, decreasing = TRUE), ]

View(df_compare_teams)

output <- function() {
  # Total games played for each team
  print(total_games_sorted)
  # Total open days for each team (higher means adding a player from that team is more beneficial)
  print(open_day_sums_sorted)
  # Total non-full days for each team on your roster (higher means losing a player from that team is more detrimental)
  print(non_full_days_sorted)
  
  print('Round Totals:')
  print(round_totals)
  print('Playoff Totals:')
  print(totals)
}

output()