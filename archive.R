# Simulate Neymon's weak null distribution
#     E[Y_i(1) - Y_i(0)] = 0, set means of Y_i(1) and Y_i(0)
#     to same value, 0, for all repetitions
# INPUT: sample size n, mean of Y_i(0) and Y_i(1) (same for both),
#        variance of Y_i(0) and Y_i(1), and number of repetitions
#        used to build the simulated null distribution
# OUTPUT: vector of length `repetitions` containing estimates of ATE_DM
#         under Neymon's weak null distribution
simulate_weak_null <- function(
        n, outcomes, mu_1,
        repetitions = 10000
) {
    outcomes$Y_1 = outcomes$Y_1 - mu_1
    ATE_DM_null <- vector(length = repetitions)
    for (rep in 1:repetitions) {
        treatments = simulate_treatments(n, pr_treatment = 0.5)
        ATE_DM_null[rep] = compute_ATE_DM(treatments, outcomes)
    }
    return(ATE_DM_null)
}
