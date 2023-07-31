position_strategic <- function(returns_num, participants_num, state_own, state_other, rounds_remaining, strategy, q = 1, scale = 1) {
    delta_state <- state_own - sort(state_other, decreasing = T)[q]
    j <- which(strategy$rounds_remaining_space == rounds_remaining)
    i <- which.min(abs(strategy$delta_state_space - delta_state))
    k <- strategy$action_function[i, j]
    position <- do.call(rbind, lapply(seq_len(participants_num), function(r) {
        position_baseline(
            returns_num = returns_num,
            participants_num = 1,
            num_shorted = strategy$position_space[k, num_shorted],
            num_zeros = strategy$position_space[k, num_zeros],
            scale = scale
        )
    }))
    return(position)
}
