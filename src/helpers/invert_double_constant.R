invert_double_constant <- function(sigma) {
    returns_num <- dim(sigma)[1]
    a <- sigma[1, 1]
    alpha <- sigma[2, 1]
    c <- 1 / alpha + returns_num * 1 / (a - alpha)
    diag_value <- 1 / (a - alpha) - 1 / (c * (a - alpha)^2)
    offdiag_value <- -1 / (c * (a - alpha)^2)
    sigma_inv <- diag_value * diag(returns_num) + offdiag_value * (matrix(1, nrow = returns_num, ncol = returns_num) - diag(returns_num))
    return(sigma_inv)
}
