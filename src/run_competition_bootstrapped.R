rm(list = ls())
library(yaml)
library(data.table)
library(MASS)

source(file.path("src", "helpers", "position_strategic.R"))
source(file.path("src", "helpers", "position_baseline.R"))
source(file.path("src", "helpers", "simulate_returns.R"))
source(file.path("src", "helpers", "compute_round.R"))
source(file.path("src", "helpers", "position_tangency.R"))
source(file.path("src", "helpers", "invert_double_constant.R"))

strategy_q_1 <- readRDS(file.path("models", "strategy_q_1.RDS"))
strategy_q_20 <- readRDS(file.path("models", "strategy_q_20.RDS"))
par <- yaml.load_file("par.yaml")
coefficients <- yaml.load_file(file.path("outputs", "metrics", "coefficients.yaml"))
returns_list <- readRDS(file.path("data", "processed", "returns_list.RDS"))
leaderboard <- readRDS(file.path("data", "processed", "leaderboard.RDS"))
leaderboard_list <- lapply(1:12, function(m) leaderboard[month == m & eligible == TRUE, .(id, ir)][order(id)])

# -- bootstrapping competition -------------------------------------------------

participant_types_measured <- c(
    "bootstrapped",
    "rank optimization (q=1)",
    "rank optimization (q=20)"
)
participant_types_other <- rep(c("bootstrapped"), c(par$participants_num_fixed - 1))
participant_types <- c(participant_types_measured, participant_types_other)
participant_rank <- matrix(
    NA,
    nrow = par$rep_simulate_competition,
    ncol = length(participant_types_measured),
    dimnames = list(NULL, participant_types_measured)
)
participant_state <- matrix(
    NA,
    nrow = par$rep_simulate_competition,
    ncol = length(participant_types_measured),
    dimnames = list(NULL, participant_types_measured)
)
bootstrapped_types <- participant_types == "bootstrapped"

set.seed(par$seed)
for (r in seq_len(par$rep_simulate_competition)) {
    if (par$rep_simulate_competition < 100 || r %% round(par$rep_simulate_competition / 100) == 0) {
        message(paste0("run_competition_bootstrapped:", round(r / par$rep_simulate_competition * 100), "%", " time:", Sys.time()))
    }
    bootstrapped_months <- sample(seq_along(leaderboard_list), 12, replace = TRUE)

    states <- rep(0, length(participant_types))
    for (rounds_remaining in rev(seq_len(par$rounds_num_fixed))) {
        m <- 12 - rounds_remaining + 1
        bootstrapped_participants <- sample(seq_len(nrow(leaderboard_list[[1]])), replace = TRUE)
        returns <- returns_list[[bootstrapped_months[[m]]]]

        positions <- matrix(NA, nrow = length(participant_types), ncol = par$returns_num_fixed)
        for (i in seq_len(nrow(positions))) {
            switch(participant_types[i],
                "rank optimization (q=1)" = {
                    positions[i, ] <- position_strategic(
                        returns_num = par$returns_num_fixed,
                        participants_num = 1,
                        state_own = states[i],
                        state_other = states[-seq_along(participant_types_measured)],
                        rounds_remaining = rounds_remaining,
                        strategy = strategy_q_1,
                        q = 1,
                        scale = 1
                    )
                },
                "rank optimization (q=20)" = {
                    positions[i, ] <- position_strategic(
                        returns_num = par$returns_num_fixed,
                        participants_num = 1,
                        state_own = states[i],
                        state_other = states[-seq_along(participant_types_measured)],
                        rounds_remaining = rounds_remaining,
                        strategy = strategy_q_20,
                        q = 20,
                        scale = 1
                    )
                }
            )
        }
        states_nonbootstrapped <- compute_round(
            returns = returns,
            positions = positions[!bootstrapped_types, ],
            states = states[!bootstrapped_types]
        )
        states_bootstrapped <- states[bootstrapped_types] + leaderboard_list[[bootstrapped_months[[m]]]][["ir"]][bootstrapped_participants]
        states[!bootstrapped_types] <- states_nonbootstrapped
        states[bootstrapped_types] <- states_bootstrapped
    }
    states_measured <- states[seq_along(participant_types_measured)]
    states_other <- states[-seq_along(participant_types_measured)]
    participant_state[r, ] <- states_measured
    participant_rank[r, ] <- sapply(states_measured, function(x) {
        rank(-c(x, states_other), ties.method = "random")[1]
    })
}

# -- saving results ------------------------------------------------------------

dir.create(file.path("models"), showWarnings = FALSE)
saveRDS(participant_rank, file.path("models", "participant_rank_bootstrapped.RDS"))
saveRDS(participant_state, file.path("models", "participant_state_bootstrapped.RDS"))

metrics <- lapply(setNames(participant_types_measured, participant_types_measured), function(participant_type) {
    list(
        mean_state = mean(participant_state[, participant_types_measured == participant_type]),
        prob_top = mean(participant_rank[, participant_types_measured == participant_type] <= 1)
    )
})
print(metrics)
dir.create(file.path("outputs", "metrics"), showWarnings = FALSE)
write_yaml(metrics, file.path("outputs", "metrics", "metrics_bootstrapped.yaml"))
