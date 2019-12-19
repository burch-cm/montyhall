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
