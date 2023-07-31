rm(list = ls())
library(yaml)
library(data.table)
library(MASS)
library(ggplot2)
library(patchwork)
library(latex2exp)

source(file.path("src", "helpers", "position_baseline.R"))
source(file.path("src", "helpers", "simulate_returns.R"))
source(file.path("src", "helpers", "compute_round.R"))

par <- yaml.load_file("par.yaml")
coefficients <- yaml.load_file(file.path("outputs", "metrics", "coefficients.yaml"))
mu <- rep(par$returns_mean, par$returns_num)
sigma <- coefficients$returns_var * diag(par$returns_num) + coefficients$returns_cov * (matrix(1, par$returns_num, par$returns_num) - diag(par$returns_num))

# -- simulating effects --------------------------------------------------------

combinations <- CJ(num_zeros = seq(0, par$returns_num - 10, 10), proportion_shorted = seq(0, 1, 0.1))
combinations[, num_shorted := as.integer((par$returns_num - num_zeros) * proportion_shorted)]

d <- do.call(rbind, lapply(seq_len(par$rep_position_effects), function(r) {
    if (par$rep_position_effects < 100 | r %% round(par$rep_position_effects / 100) == 0) {
        message(paste0("position_effects:", round(r / par$rep_position_effects * 100), "%", " time:", Sys.time()))
    }
    set.seed(r)
    returns_list <- simulate_returns(
        days_per_round = par$days_per_round,
        mu = mu,
        sigma = sigma,
        lambda = 0
    )
    returns <- returns_list[[1]]
    state_baseline <- compute_round(
        returns,
        position_baseline(
            returns_num = par$returns_num,
            participants_num = 1,
            num_shorted = coefficients$num_shorted,
            num_zeros = coefficients$num_zeros
        )
    )
    state <- sapply(seq_len(nrow(combinations)), function(i) {
        num_shorted <- combinations[i, num_shorted]
        num_zeros <- combinations[i, num_zeros]
        positions <- position_baseline(
            returns_num = par$returns_num,
            participants_num = 1,
            num_shorted = num_shorted,
            num_zeros = num_zeros
        )
        compute_round(returns, positions)
    })
    cbind(r = r, combinations, state = state, state_diff = state - state_baseline)
}))
dm <- melt(d, measure.var = c("state", "state_diff"))
dma <- dm[
    , .(
        statistic = c("mean", "sd", "q95"),
        value = c(mean(value), sd(value), quantile(value, .95))
    ),
    .(num_zeros, proportion_shorted, variable)
]

# -- plotting ------------------------------------------------------------------

dma$variable <- factor(
    dma$variable,
    levels = c("state", "state_diff"),
    labels = c(
        TeX("$IR_{T_{m},k}|n_{+},n_{-}$"),
        TeX("$(IR_{T_{m},k}|n_{+},n_{-})-(IR_{T_{m},k}|\\widehat{n}_{+},\\widehat{n}_{-})$")
    )
)

plot_mean <- ggplot(dma[statistic == "mean"], aes(x = num_zeros, y = proportion_shorted, fill = value)) +
    geom_tile() +
    facet_wrap(variable ~ ., scales = "free", ncol = 1, labeller = label_parsed) +
    scale_fill_gradient(low = "red", high = "green") +
    theme(legend.position = "bottom") +
    labs(x = TeX("$n_{0}$"), y = TeX("$\\frac{n_{-}}{n_{+}+n_{-}}$"), fill = TeX("$mean$"))

plot_sd <- ggplot(dma[statistic == "sd"], aes(x = num_zeros, y = proportion_shorted, fill = value)) +
    geom_tile() +
    facet_wrap(variable ~ ., scales = "free", ncol = 1, labeller = label_parsed) +
    scale_fill_gradient(low = "red", high = "green") +
    theme(legend.position = "bottom") +
    labs(y = NULL, x = TeX("$n_{0}$"), fill = TeX("$sd$"))

plot_q95 <- ggplot(dma[statistic == "q95"], aes(x = num_zeros, y = proportion_shorted, fill = value)) +
    geom_tile() +
    facet_wrap(variable ~ ., scales = "free", ncol = 1, labeller = label_parsed) +
    scale_fill_gradient(low = "red", high = "green") +
    theme(legend.position = "bottom") +
    labs(y = NULL, x = TeX("$n_{0}$"), fill = TeX("$q_{0.95}$"))

position_effects <- plot_mean + plot_sd + plot_q95

dir.create(file.path("outputs", "plots"), showWarnings = FALSE)
ggsave(file.path("outputs", "plots", "position_effects.png"), position_effects, width = 7, height = 6)
