# computational/R/functions/model.R
# Single-agent reduced-form functions for the two-stage model.
# Opponent strength S is exogenous. Action a is binary --
#     - functions ending in 0 correspond to a=0
#     - functions ending in 1 correspond to a=1
#
# No file I/O in this file.

# -------------------------
# Validation helpers
# -------------------------

# check delta > 0
check_delta <- function(delta) {
    if (length(delta) != 1 || !is.finite(delta) || delta <= 0) {
      stop("delta must be a single finite number > 0.")
    }
    invisible(TRUE)
}

# check S > 0
check_S <- function(S) {
  if (any(!is.finite(S)) || any(S <= 0)) {
    stop("S must be finite and strictly positive.")
  }
  invisible(TRUE)
}

# -------------------------
# Distribution primitives
# -------------------------

Phi_std <- function(x) stats::pnorm(x)    # standard normal CDF
phi_std <- function(x) stats::dnorm(x)    # standard normal PDF

# -------------------------
# Primary stage reduced-form objects
# -------------------------

# Nomination probability under a=0:
# p0(v;S) = exp(v) / (exp(v) + S)
p0 <- function(v, S) {
  check_S(S)
  num <- exp(v)
  num / (num + S)
}

# Nomination probability under a=1:
# p0(v;S) = exp(v + 1) / (exp(v + 1) + S)
p1 <- function(v, S) {
  check_S(S)
  num <- exp(v + 1)
  num / (num + S)
}

# Proportional nomination gain:
# G(v;S) = p1/p0
G_gain <- function(v, S) {
  check_S(S)
  p1(v, S) / p0(v, S)
}

# -------------------------
# General election objects
# -------------------------

# General election win prob under a=0: Phi(v)
ge0 <- function(v) {
  Phi_std(v)
}

# General election win prob under a=1: Phi(v - delta)
ge1 <- function(v, delta) {
  check_delta(delta)
  Phi_std(v - delta)
}

# Proportional electability cost: 
# C(v; delta) = Phi(v) / Phi(v-delta)
C_cost <- function(v, delta) {
  check_delta(delta)
  Phi_std(v) / Phi_std(v - delta)
}

# -------------------------
# Payoffs (expected utility) under each action
# -------------------------

# u0(v; S, delta) = p0(v; S) * Phi(v)
u0 <- function(v, S) {
  check_S(S)
  p0(v, S) * Phi_std(v)
}

# u1(v; S, delta) = p1(v; S) * Phi(v - delta)
u1 <- function(v, S, delta) {
  check_S(S)
  check_delta(delta)
  p1(v, S) * Phi_std(v - delta)
}

# Incentive difference:
# Delta(v; S, delta) = u1 - u0
Delta_incentive <- function(v, S, delta) {
  check_S(S)
  check_delta(delta)
  u1(v, S, delta) - u0(v, S)
}

# Best response rule: choose a=1 iff Delta(v; S, delta) >= 0
choose_a <- function(v, S, delta, tie_break = c("one", "zero")) {
  tie_break <- match.arg(tie_break)
  d <- Delta_incentive(v, S, delta)
  ifelse(d > 0, 1L, ifelse(d < 0, 0L, if (tie_break == "one") 1L else 0L))
}
