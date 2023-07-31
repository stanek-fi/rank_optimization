rm(list = ls())
library(yaml)
library(data.table)
library(stringr)
library(MASS)
library(moments)

source(file.path("src", "helpers", "position_baseline.R"))
source(file.path("src", "helpers", "compute_round.R"))

par <- yaml.load_file("par.yaml")
returns <- readRDS(file.path("data", "processed", "returns.RDS"))
returns_list <- readRDS(file.path("data", "processed", "returns_list.RDS"))
leaderboard <- readRDS(file.path("data", "processed", "leaderboard.RDS"))

# -- estimating vcov -----------------------------------------------------------

returns_matrix <- as.matrix(returns[, -1])
covariance_matrix <- cov(returns_matrix, use = "pairwise.complete.obs")
returns_var <- round(mean(diag(covariance_matrix)), 5)
returns_cov <- round(mean(covariance_matrix[lower.tri(covariance_matrix)]), 5)

# -- esimating num_shorted and num_zeros ---------------------------------------

leaderboard_moments <- leaderboard[eligible == TRUE, .(ir_mean = mean(ir), ir_kurt = kurtosis(ir)), month]
combinations <- CJ(num_zeros = seq(0, par$returns_num_fixed - 1, 1), num_shorted = seq(0, par$returns_num_fixed, 1))
combinations <- combinations[!(par$returns_num_fixed - num_zeros < pmax(0, num_shorted))]
weights <- diag(2)

out <- rbindlist(lapply(seq_len(nrow(combinations)), function(i) {
    if (nrow(combinations) < 100 | i %% round(nrow(combinations) / 100) == 0) {
        message(paste0("estimate_coefficients:", round(i / nrow(combinations) * 100), "%", " time:", Sys.time()))
    }
    num_shorted <- combinations[i, num_shorted]
    num_zeros <- combinations[i, num_zeros]
    temp <- rbindlist(lapply(1:par$rounds_num_fixed, function(i) {
        returns <- returns_list[[i]]
        temp <- rbindlist(lapply(1:par$rep_estimate_coefficients, function(r) {
            set.seed(r)
            positions <- position_baseline(par$returns_num_fixed, par$participants_num_fixed, num_shorted, num_zeros)
            states <- compute_round(returns, positions)
            list(
                state_mean = mean(states),
                state_kurt = kurtosis(states)
            )
        }))
        list(
            state_mean_mean = mean(temp$state_mean),
            state_kurt_mean = mean(temp$state_kurt),
            state_mean_var = var(temp$state_mean),
            state_kurt_var = var(temp$state_kurt)
        )
    }))

    sq_dev <- (temp[, .(state_mean_mean, state_kurt_mean)] - leaderboard_moments[, .(ir_mean, ir_kurt)])^2
    sq_dev_mean <- temp[, .(state_mean_var, state_kurt_var)]
    sq_dev_sd <- sqrt(temp[, .(state_mean_var, state_kurt_var)])
    moments <- (sq_dev - sq_dev_mean) / sq_dev_sd
    moments_mean <- apply(moments, 2, mean)
    loss <- as.vector(t(moments_mean) %*% weights %*% moments_mean)

    list(
        num_shorted = num_shorted,
        num_zeros = num_zeros,
        loss = loss
    )
}))

# -- saving coefficients -------------------------------------------------------

coefficients <- list(
    num_shorted = out[order(loss)][1, num_shorted],
    num_zeros = out[order(loss)][1, num_zeros],
    returns_var = returns_var,
    returns_cov = returns_cov
)
print(coefficients)
dir.create(file.path("outputs", "metrics"), showWarnings = FALSE)
write_yaml(coefficients, file.path("outputs", "metrics", "coefficients.yaml"))
