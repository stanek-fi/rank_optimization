library(data.table)
library(ggplot2)
library(scales)

participant_rank <- readRDS(file.path("models", "participant_rank_bootstrapped.RDS"))
participant_state <- readRDS(file.path("models", "participant_state_bootstrapped.RDS"))

# -- rank histogram ------------------------------------------------------------

participant_rank_dt <- as.data.table(participant_rank)
participant_rank_long <- melt(participant_rank_dt, measure.vars = colnames(participant_rank_dt), variable.name = "portfolio", value.name = "rank")
participant_state_dt <- as.data.table(participant_state)
participant_state_long <- melt(participant_state_dt, measure.vars = colnames(participant_state_dt), variable.name = "portfolio", value.name = "state")
participant_rank_long_subset <- participant_rank_long[
    portfolio %in% c("rank optimization (q=1)", "rank optimization (q=20)", "bootstrapped")
]

rank_histogram <- ggplot(participant_rank_long_subset, aes(rank, fill = portfolio)) +
    geom_histogram(aes(y = after_stat(count / sum(count[group == 1]))), binwidth = 1, alpha = .5, position = "identity") +
    ylab("frequency") +
    scale_fill_manual(values = hue_pal()(4)[-2])+
    theme(legend.position="bottom", legend.title=element_blank())
dir.create(file.path("outputs", "plots"), showWarnings = FALSE)
ggsave(file.path("outputs", "plots", "rank_histogram_bootstrapped.png"), rank_histogram, width = 6.6, height = 6)

# -- state table ---------------------------------------------------------------

rank_table <- participant_rank_long[, .(top_1 = mean(rank == 1), top_5 = mean(rank <= 5), top_10 = mean(rank <= 10), top_20 = mean(rank <= 20)), .(portfolio)]
state_table <- participant_state_long[, .(mean = mean(state)), portfolio]
state_table <- merge(state_table, rank_table, by = "portfolio")
print(state_table)
dir.create(file.path("outputs", "tables"), showWarnings = FALSE)
write.csv(state_table, file.path("outputs", "tables", "state_table_bootstrapped.csv"), row.names = FALSE)
