simulate_returns <- function(days_per_round, mu, sigma, lambda = 0) {
    returns_predictable <- mvrnorm(
        n = days_per_round,
        mu = lambda * mu,
        Sigma = sqrt(lambda)^2 * sigma
    )
    returns_unpredictable <- mvrnorm(
        n = days_per_round,
        mu = (1 - lambda) * mu,
        Sigma = sqrt(1 - lambda)^2 * sigma
    )
    returns_total <- returns_predictable + returns_unpredictable
    list(
        returns_total = returns_total,
        returns_predictable = returns_predictable,
        returns_unpredictable = returns_unpredictable
    )
}
