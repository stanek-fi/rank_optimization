rm(list = ls())
library(yaml)
library(data.table)
library(MASS)

source(file.path("src", "helpers", "compute_round.R"))
source(file.path("src", "helpers", "simulate_returns.R"))
source(file.path("src", "helpers", "position_baseline.R"))

par <- yaml.load_file("par.yaml")
coefficients <- yaml.load_file(file.path("outputs", "metrics", "coefficients.yaml"))
mu <- rep(par$returns_mean, par$returns_num)
sigma <- coefficients$returns_var * diag(par$returns_num) + coefficients$returns_cov * (matrix(1, par$returns_num, par$returns_num) - diag(par$returns_num))
rep_own <- 100
rep_other <- 100
qs <- c(1, 20)

# -- simulating realizations ---------------------------------------------------

position_space <- CJ(
    num_shorted = seq(0, par$returns_num, 10),
    num_zeros = 0
)
position_space <- position_space[(par$returns_num - num_zeros >= pmax(num_shorted, 1))]
delta_state_space <- seq(-20, 20, 1)
rounds_remaining_space <- seq(0, par$rounds_num)
value_function <- matrix(NA, nrow = length(delta_state_space), ncol = length(rounds_remaining_space))
action_function <- matrix(NA, nrow = length(delta_state_space), ncol = length(rounds_remaining_space))

set.seed(par$seed)
realizations <- lapply(seq_len(par$rep_compute_strategy), function(r) {
    if (par$rep_compute_strategy < 100 | r %% round(par$rep_compute_strategy / 100) == 0) {
        message(paste0("compute_strategy: realizations:", round(r / par$rep_compute_strategy * 100), "%", " time:", Sys.time()))
    }
    returns_list <- lapply(seq_len(par$rounds_num), function(m) {
        simulate_returns(days_per_round = par$days_per_round, mu = mu, sigma = sigma, lambda = 0)$returns_total
    })
    states_cum_qth_list <- lapply(seq_len(rep_other), function(r) {
        states <- do.call(rbind, lapply(seq_len(par$rounds_num), function(m) {
            positions <- position_baseline(
                returns_num = par$returns_num,
                participants_num = par$participants_num - 1,
                num_shorted = coefficients$num_shorted,
                num_zeros = coefficients$num_zeros
            )
            compute_round(returns = returns_list[[m]], positions = positions)
        }))
        states_cum <- apply(states, 2, cumsum)
        t(apply(states_cum, 1, function(x) sort(x, decreasing = TRUE)[qs]))
    })
    states_cum_qth_list <- lapply(seq_along(qs), function(i) {
        mapply("[", states_cum_qth_list, TRUE, i)
    })
    states_qth_list <- lapply(seq_along(qs), function(i) {
        rbind(states_cum_qth_list[[i]][1, ], apply(states_cum_qth_list[[i]], 2, diff))
    })
    list(
        returns_list = returns_list,
        states_qth_list = states_qth_list
    )
})

# -- computing strategies ------------------------------------------------------

for (qi in seq_along(qs)) {
    for (mr in seq_along(rounds_remaining_space)) {
        message(paste0("compute_strategy:", " q:", qs[qi], " rounds_remaining:", rounds_remaining_space[mr], " time:", Sys.time()))
        m <- par$rounds_num - rounds_remaining_space[mr] + 1
        if (rounds_remaining_space[mr] == 0) {
            value_function[, mr] <- as.numeric(delta_state_space > 0)
        } else {
            action_value_functions <- sapply(seq_len(nrow(position_space)), function(a) {
                value_realizations <- sapply(seq_len(par$rep_compute_strategy), function(r) {
                    set.seed(r)
                    positions <- position_baseline(
                        returns_num = par$returns_num,
                        participants_num = rep_own,
                        num_shorted = position_space[a, num_shorted],
                        num_zeros = position_space[a, num_zeros],
                    )
                    states_own <- compute_round(returns = realizations[[r]]$returns_list[[m]], positions = positions)
                    states_qth <- realizations[[r]]$states_qth_list[[qi]][m, ]
                    states_diff <- do.call(c, lapply(states_qth, function(x) states_own - x))
                    sapply(delta_state_space, function(delta_state) {
                        x_min <- value_function[1, mr - 1] / ((value_function[2, mr - 1] - value_function[1, mr - 1]) / (delta_state_space[2] - delta_state_space[1]))
                        x_max_numerator <- (1 - value_function[nrow(value_function), mr - 1]) * (delta_state_space[nrow(value_function)] - delta_state_space[nrow(value_function) - 1])
                        x_max_denominator <- value_function[nrow(value_function), mr - 1] - value_function[nrow(value_function) - 1, mr - 1]
                        x_max <- x_max_numerator / x_max_denominator
                        mean(approx(
                            x = c(min(delta_state_space) - ifelse(is.na(x_min), 1, x_min), delta_state_space, max(delta_state_space) + ifelse(is.na(x_max), 1, x_max)),
                            y = c(0, value_function[, mr - 1], 1),
                            xout = states_diff + delta_state,
                            rule = 2
                        )$y)
                    })
                })
                apply(value_realizations, 1, mean)
            })
            value_function[, mr] <- apply(action_value_functions, 1, max)
            action_function[, mr] <- apply(action_value_functions, 1, which.max)
        }
    }

    strategy <- list(
        position_space = position_space,
        delta_state_space = delta_state_space,
        rounds_remaining_space = rounds_remaining_space,
        value_function = value_function,
        action_function = action_function
    )
    print(strategy)
    dir.create(file.path("models"), showWarnings = FALSE)
    saveRDS(strategy, file.path("models", paste0("strategy_q_", qs[qi], ".RDS")))
}
