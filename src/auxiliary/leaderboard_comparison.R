rm(list = ls())
library(yaml)
library(data.table)
library(MASS)

source(file.path("src", "helpers", "position_baseline.R"))
source(file.path("src", "helpers", "simulate_returns.R"))
source(file.path("src", "helpers", "compute_round.R"))

par <- yaml.load_file("par.yaml")
coefficients <- yaml.load_file(file.path("outputs", "metrics", "coefficients.yaml"))
returns_list <- readRDS(file.path("data", "processed", "returns_list.RDS"))
leaderboard <- readRDS(file.path("data", "processed", "leaderboard.RDS"))

# -- helper functions ----------------------------------------------------------

statistics <- function(x) {
    list(
        mean = mean(x),
        sd = sd(x),
        q_01 = quantile(x, 0.01, type = 6),
        q_99 = quantile(x, 0.99, type = 6)
    )
}

rf <- function(x, p) {
    formatC(round(x, p), format = "f", digits = p)
}

# -- realized_statistics -------------------------------------------------------

temp <- rbind(
    leaderboard[eligible == TRUE, .(id, ir, month)],
    leaderboard[eligible == TRUE, .(ir = sum(ir), month = "total"), id]
)
statistics_realized <- temp[, statistics(ir), month]
statistics_realized <- statistics_realized[, lapply(.SD, function(x) rf(x, 2)), month]

# -- simulated_statistics ------------------------------------------------------

set.seed(par$seed)
statistics_simulated <- lapply(1:par$rep_leaderboard_comparison, function(r) {
    if (par$rep_leaderboard_comparison < 100 | r %% round(par$rep_leaderboard_comparison / 100) == 0) {
        message(paste0("leaderboard_comparison:", round(r / par$rep_leaderboard_comparison * 100), "%", " time:", Sys.time()))
    }
    temp <- do.call(rbind, lapply(1:par$rounds_num_fixed, function(i) {
        returns <- returns_list[[i]]
        positions <- position_baseline(
            returns_num = par$returns_num_fixed,
            participants_num = par$participants_num_fixed,
            num_shorted = coefficients$num_shorted,
            num_zeros = coefficients$num_zeros
        )
        states <- compute_round(returns, positions)
        states
    }))
    temp <- rbind(temp, colSums(temp))
    temp <- rbindlist(apply(temp, 1, statistics))
    temp[, month := c(1:par$rounds_num_fixed, "total")]
    temp
})
statistics_simulated <- do.call(rbind, statistics_simulated)
statistics_simulated <- statistics_simulated[, lapply(.SD, function(x) {
    list(rf(mean(x), 2), paste0("(", rf(sd(x), 2), ")"))
}), month]

# -- merging it together -------------------------------------------------------

statistics_realized <- statistics_realized[rep(1:.N, each = 2), ]
statistics_realized[seq(1, .N, 2) + 1, seq_len(ncol(statistics_realized)) := ""]
leaderboard_comparison <- cbind(statistics_realized, statistics_simulated)
new_order <- do.call(c, lapply(seq_len(ncol(statistics_realized)), function(x) x + c(0, ncol(statistics_realized))))
setcolorder(leaderboard_comparison, new_order)
names(leaderboard_comparison) <- paste0(rep(c("obs_", "sim_"), times = ncol(leaderboard_comparison) / 2), names(leaderboard_comparison))
leaderboard_comparison <- leaderboard_comparison[, -2]
print(leaderboard_comparison)
dir.create(file.path("outputs", "tables"), showWarnings = FALSE)
write.csv(as.matrix(leaderboard_comparison), file.path("outputs", "tables", "leaderboard_comparison.csv"), row.names = FALSE)
