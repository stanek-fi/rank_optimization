position_tangency <- function(mu, sigma, returns_predictable, lambda, scale = 1) {
    mu_predictable <- apply(returns_predictable, 2, mean)
    sigma_inv <- invert_double_constant((1 - lambda) * sigma)
    position <- sigma_inv %*% ((1 - lambda) * mu + mu_predictable)
    position <- position / sum(abs(position)) * scale
    position <- t(position)
    return(position)
}
