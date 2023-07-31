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
mu <- rep(par$returns_mean, par$returns_num)
sigma <- coefficients$returns_var * diag(par$returns_num) + coefficients$returns_cov * (matrix(1, par$returns_num, par$returns_num) - diag(par$returns_num))

# -- simulating competition ----------------------------------------------------

participant_types_measured <- c(
    "baseline",
    "tangency (without insider info)",
    "tangency (with insider info)",
    "rank optimization (q=1)",
    "rank optimization (q=20)"
)
participant_types_other <- rep(c("baseline"), c(par$participants_num - 1))
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

set.seed(par$seed)
for (r in seq_len(par$rep_simulate_competition)) {
    if (par$rep_simulate_competition < 100 || r %% round(par$rep_simulate_competition / 100) == 0) {
        message(paste0("run_competition_simulated:", round(r / par$rep_simulate_competition * 100), "%", " time:", Sys.time()))
    }

    states <- rep(0, length(participant_types))
    for (rounds_remaining in rev(seq_len(par$rounds_num))) {
        returns_list <- simulate_returns(
            days_per_round = par$days_per_round,
            mu = mu,
            sigma = sigma,
            lambda = par$returns_lambda
        )

        positions <- matrix(NA, nrow = length(participant_types), ncol = par$returns_num)
        for (i in seq_len(nrow(positions))) {
            switch(participant_types[i],
                "baseline" = {
                    positions[i, ] <- position_baseline(
                        returns_num = par$returns_num,
                        participants_num = 1,
                        num_shorted = coefficients$num_shorted,
                        num_zeros = coefficients$num_zeros
                    )
                },
                "tangency (with insider info)" = {
                    positions[i, ] <- position_tangency(
                        mu = mu,
                        sigma = sigma,
                        returns_predictable = returns_list$returns_predictable,
                        lambda = par$returns_lambda
                    )
                },
                "tangency (without insider info)" = {
                    positions[i, ] <- position_tangency(
                        mu = mu,
                        sigma = sigma,
                        returns_predictable = returns_list$returns_predictable * 0,
                        lambda = 0
                    )
                },
                "rank optimization (q=1)" = {
                    positions[i, ] <- position_strategic(
                        returns_num = par$returns_num,
                        participants_num = 1,
                        state_own = states[i],
                        state_other = states[-seq_along(participant_types_measured)],
                        rounds_remaining = rounds_remaining,
                        strategy = strategy_q_1,
                        q = 1
                    )
                },
                "rank optimization (q=20)" = {
                    positions[i, ] <- position_strategic(
                        returns_num = par$returns_num,
                        participants_num = 1,
                        state_own = states[i],
                        state_other = states[-seq_along(participant_types_measured)],
                        rounds_remaining = rounds_remaining,
                        strategy = strategy_q_20,
                        q = 20
                    )
                }
            )
        }
        states <- compute_round(
            returns = returns_list$returns_total,
            positions = positions,
            states = states
        )
    }
    states_measured <- states[seq_along(participant_types_measured)]
    states_other <- states[-seq_along(participant_types_measured)]
    participant_state[r, ] <- states_measured
    participant_rank[r, ] <- sapply(states_measured, function(x) {
        rank(-c(x, states_other))[1]
    })
}

# -- saving results ------------------------------------------------------------

dir.create(file.path("models"), showWarnings = FALSE)
saveRDS(participant_rank, file.path("models", "participant_rank_simulated.RDS"))
saveRDS(participant_state, file.path("models", "participant_state_simulated.RDS"))

metrics <- lapply(setNames(participant_types_measured, participant_types_measured), function(participant_type) {
    list(
        mean_state = mean(participant_state[, participant_types_measured == participant_type]),
        prob_top = mean(participant_rank[, participant_types_measured == participant_type] <= 1)
    )
})
print(metrics)
dir.create(file.path("outputs", "metrics"), showWarnings = FALSE)
write_yaml(metrics, file.path("outputs", "metrics", "metrics_simulated.yaml"))
