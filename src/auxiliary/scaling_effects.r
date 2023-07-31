rm(list = ls())
library(data.table)
library(MASS)
library(ggplot2)
library(latex2exp)

source(file.path("src", "helpers", "position_baseline.R"))
source(file.path("src", "helpers", "simulate_returns.R"))
source(file.path("src", "helpers", "compute_round.R"))

returns_list <- readRDS(file.path("data", "processed", "returns_list.RDS"))
num_shorted <- 0
num_zeros <- 0
scalings <- seq(0, 1, by = 0.001)[-1]

# -- computing effects ---------------------------------------------------------

res <- do.call(rbind, lapply(1:12, function(m) {
    returns <- returns_list[[m]]
    positions <- position_baseline(ncol(returns), 1, num_shorted = num_shorted, num_zeros = num_zeros)
    positions_scaled <- as.matrix(scalings) %*% positions
    data.table(
        month = m,
        scale = scalings,
        IR = compute_round(returns, positions_scaled),
        admissible = (scalings >= 0.25) & (scalings <= 1)
    )
}))
res[, month := as.factor(month)]

# -- plotting effects ----------------------------------------------------------

res[, IR := IR - .SD[scale == 1, IR], month]
optimal_scaling <- ggplot(res, aes(x = scale, y = IR, colour = month, linetype = admissible)) +
    geom_line() +
    scale_linetype_manual(values = c("dashed", "solid")) +
    ylab(TeX("($IR_{T_{m},k}|\\alpha_{m,k}=alpha) - (IR_{T_{m},k}|\\alpha_{m,k}=1)$")) +
    xlab(TeX("$\\alpha$")) +
    labs(color = "m")
dir.create(file.path("outputs", "plots"), showWarnings = FALSE)
ggsave(file.path("outputs", "plots", "scaling_effects.png"), optimal_scaling, width = 6, height = 5)
