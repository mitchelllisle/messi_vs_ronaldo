createAllGoals <- function(messiGoals, ronaldoGoals){
  goalsPerGameMatch <- rbind(messiGoals, ronaldoGoals) %>% select(Year, player, TotalGoals, TotalApps) %>% 
    mutate(goalsPerGame = round(TotalGoals/TotalApps, 2)) %>%
    select(Year, player, goalsPerGame) %>%
    tidyr::spread(player, goalsPerGame) %>%
    mutate(winner = if_else(Messi > Ronaldo, "Messi", "Ronaldo")) %>% 
    group_by(winner) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) 
  
  return(goalsPerGameMatch)
}