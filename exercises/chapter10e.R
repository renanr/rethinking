# EASY
# 1
# p = 0.35
log_odds = log(0.35 / (1 - 0.35))
log_odds

# 2
# log-odds = 3.2
p = exp(3.2) / (1 + exp(3.2))
p

# 3
plot(logistic(seq(from=-17, to=17, by=1.7)) ~ seq(21))

# 4
# Because frequencies might be expressed in different periods.
# For instance, RPG characters' damage in damage/hit or damage/second.

# MEDIUM
# 1
# The likelihood changes because the aggregated form loses information about order,
# so that is not accounted in the likelihood.

# 2
plot(exp(seq(from=-17, to=17, by=1.7)) ~ seq(21))

# 3
# Because it maps the real numbers onto values between 0 and 1, matching them to probabilites.

# 4
# Because it maps the real numbers onto unbounded positive values, matching them to rates.

# 5
# It would imply that rates are bounded between 0 and 1. Maybe portion of time when an alarm
# went off (not as a count, but as period_alarm / total_period)?

# 6
# Constraints: count outcome that comes from N trials with CONSTANT expected value n * p.
# The constraints are equal because Poisson is just a special case of the binomial.