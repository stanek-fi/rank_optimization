rm(list = ls())
library(data.table)
library(ggplot2)
library(yaml)
library(latex2exp)

par <- yaml.load_file("par.yaml")

qs <- c(1, 20)
for (q in qs) {
    strategy <- readRDS(file.path("models", paste0("strategy_q_", q, ".RDS")))

    # -- preparation ---------------------------------------------------------------

    num_shorted <- matrix(
        strategy$position_space[c(strategy$action_function), num_shorted],
        nrow = nrow(strategy$action_function),
        dimnames = list(delta_state = strategy$delta_state_space, rounds_remaining = strategy$rounds_remaining_space)
    )
    num_zeros <- matrix(
        strategy$position_space[c(strategy$action_function), num_zeros],
        nrow = nrow(strategy$action_function),
        dimnames = list(delta_state = strategy$delta_state_space, rounds_remaining = strategy$rounds_remaining_space)
    )
    action_funcion_long <- cbind(
        as.data.table(do.call(expand.grid, dimnames(num_shorted))),
        num_shorted = as.vector(num_shorted),
        num_zeros = as.vector(num_zeros)
    )
    action_funcion_long <- action_funcion_long[rounds_remaining != 0]
    action_funcion_long[, num_stocks := par$returns_num_fixed - num_zeros]
    action_funcion_long[, long := num_stocks - num_shorted]
    action_funcion_long[, short := num_shorted]

    # -- plotting ------------------------------------------------------------------

    temp <- melt(action_funcion_long, id.vars = c("delta_state", "rounds_remaining"), measure.vars = c("short", "long"), variable.name = "position")
    temp <- temp[, t := 1 + par$rounds_num - as.numeric(as.character(rounds_remaining))]
    temp[, delta_state := factor(delta_state, levels = sort(unique(delta_state), decreasing = TRUE))]
    temp <- temp[as.numeric(as.character(delta_state)) %% 2 == 0]
    temp <- temp[as.numeric(as.character(delta_state)) < 15 & as.numeric(as.character(delta_state)) > -15]
    p <- ggplot(temp, aes(x = "", y = value, fill = position)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        facet_grid(delta_state ~ t, switch = "both") +
        theme(axis.text.x = element_blank(), axis.ticks = element_blank(), legend.position = "bottom") +
        labs(x = TeX("$\\Delta_{m}$"), y = "m")
    dir.create(file.path("outputs", "plots"), showWarnings = FALSE)
    ggsave(file.path("outputs", "plots", paste0("strategy_diagram_q_", q, ".png")), p, width = 6, height = 7.5)
}
