rm(list = ls())
library(data.table)
library(stringr)
library(yaml)

par <- yaml.load_file("par.yaml")

# -- processing data -----------------------------------------------------------

prices <- as.data.table(read.csv(file.path("data", "raw", "prices", "assets_M6.csv")))
prices[, date := as.Date(date, "%Y/%m/%d")]
prices <- dcast(prices, date ~ symbol, value.var = "price")
returns <- prices[, .(date, .SD / shift(.SD) - 1), .SDcols = colnames(prices)[-1]]

intervals <- list(
    start = seq(as.Date("2022-03-07"), by = 28, length.out = 12),
    end = seq(as.Date("2022-04-03"), by = 28, length.out = 12)
)
returns_list <- lapply(seq_along(intervals$start), function(i) {
    temp <- returns[date >= intervals$start[i] & date <= intervals$end[i], .SD, .SDcols = colnames(returns)[-1]]
    temp <- as.matrix(temp)
    temp[is.na(temp)] <- 0
    temp
})

leaderboard <- do.call(rbind, lapply(1:par$rounds_num_fixed, function(i) {
    temp <- as.data.table(
        read.csv(
            file.path("data", "raw", "leaderboard", paste0(i, ".csv")),
            header = FALSE,
            sep = ";",
            col.names = c("rank", "name", "rank_avg", "rps", "rank_rps", "ir", "rank_ir")
        )
    )
    temp[, id := word(name)]
    temp[, month := i]
    temp
}))
leaderboard[, eligible := .N == par$rounds_num_fixed, id]

# -- saving results -----------------------------------------------------------

dir.create(file.path("data", "processed"), showWarnings = FALSE)
saveRDS(returns, file.path("data", "processed", "returns.RDS"))
saveRDS(returns_list, file.path("data", "processed", "returns_list.RDS"))
saveRDS(leaderboard, file.path("data", "processed", "leaderboard.RDS"))
