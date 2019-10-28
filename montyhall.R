library(dplyr)
library(purrr)


simple_hall <- function(strategy = "stay") {
    stay <- sample(c("car", "goat", "goat"), 1)
    if (stay == "car") {
        switch <- "goat"
    } else {
        switch <- "car"
    }
    if (strategy == "stay") return(stay)
    return(switch)
}

run_halls <- function(n_halls, percentage = FALSE) {
    stay_   <- purrr::map_chr(rep("stay", n_halls), simple_hall)
    switch_ <- purrr::map_chr(rep("switch", n_halls), simple_hall)
    stay_wins <- table(stay_)["car"]
    switch_wins <- table(switch_)["car"]
    if (percentage) {
        stay_wins <- stay_wins / n_halls
        switch_wins <- switch_wins / n_halls
    }
    return(c(stay_wins = stay_wins, switch_wins = switch_wins))
}
