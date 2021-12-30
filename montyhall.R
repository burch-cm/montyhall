library(dplyr)
library(purrr)


simple_hall <- function(strategy = "stay") {
    stay <- sample(factor(c("car", "goat", "goat")), 1)
    if (stay == "car") {
        switch <- factor(c("goat"), levels = c("car", "goat"))
    } else {
        switch <- factor(c("car"), levels = c("car", "goat"))
    }
    if (strategy == "stay") return(stay)
    return(switch)
}

run_halls <- function(n_halls, percentage = FALSE) {
    stay_   <- purrr::map_int(rep("stay", n_halls), simple_hall)
    switch_ <- purrr::map_int(rep("switch", n_halls), simple_hall)
    stay_wins <- table(stay_)[1]
    switch_wins <- table(switch_)[1]
    if (percentage) {
        stay_wins <- stay_wins / n_halls
        switch_wins <- switch_wins / n_halls
    }
    return(c(stay_wins = stay_wins, switch_wins = switch_wins))
}

#####

# set up game
set_game <- function(win = "win", lose = "lose", n_doors = 3) {
    stopifnot(is.numeric(n_doors),
              n_doors <= 1)
    goats <- rep(lose, (n_doors - 1))
    doors <- c(goats, win)
    sample(doors, n_doors, replace = FALSE)
}

# 
reveal_goat <- function(game, guess, win = "win", lose = "lose") {
    # get index of win door
    win_door <- which(game == win)
    # 
}

# play game

make_guess <- function(game, door = NA, verbose = FALSE) {
    # if no guess provided, make a random guess
    if (is.na(door)) door <- sample(1:length(game))
    
    if (verbose) print(paste("You choose door number", door))
    
    # the host will reveal a non-winning door, no matter which door you pick
    
    
    
}