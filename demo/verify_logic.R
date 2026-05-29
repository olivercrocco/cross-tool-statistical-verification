# verify_logic.R — the verification logic behind the live demo.
#
# Pure base R (no Shiny), so it can be sourced and tested on its own. The demo
# reproduces the bundled mtcars example (OLS mpg ~ wt + hp): the R side is
# computed live, and the Python side is the verified output of analysis.py
# (statsmodels), held here as a fixed reference.

# --- Python reference (verified statsmodels output of analysis.py) -----------
python_ref <- list(
  n_obs          = 32,
  model_r2       = 0.8267855,
  coef_intercept = 37.22727,
  coef_wt        = -3.877831,
  coef_hp        = -0.03177295,
  p_wt           = 1.119647e-06,
  p_hp           = 0.001451229,
  resid_sum      = 0,            # OLS residuals sum to ~0
  mean_mpg       = 20.09062
)

STAT_ORDER <- c("n_obs", "model_r2", "coef_intercept", "coef_wt", "coef_hp",
                "p_wt", "p_hp", "resid_sum", "mean_mpg")

# --- The R replication, computed live ----------------------------------------
# mode: "correct"    -> mpg ~ wt + hp  (matches the Python analysis)
#       "dropped"    -> mpg ~ wt       (a predictor is missing; hp stats absent)
#       "mislabeled" -> mpg ~ wt + qsec, but qsec's coefficient is reported
#                       under the hp name (a realistic column-mapping bug)
r_results <- function(mode = "correct") {
  d <- mtcars
  fit <- switch(mode,
    dropped    = lm(mpg ~ wt, data = d),
    mislabeled = lm(mpg ~ wt + qsec, data = d),
    lm(mpg ~ wt + hp, data = d))
  co <- summary(fit)$coefficients
  getc <- function(term, col) if (term %in% rownames(co)) co[term, col] else NA_real_

  hp_term <- if (mode == "mislabeled") "qsec" else "hp"
  list(
    n_obs          = nrow(d),
    model_r2       = summary(fit)$r.squared,
    coef_intercept = getc("(Intercept)", "Estimate"),
    coef_wt        = getc("wt", "Estimate"),
    coef_hp        = getc(hp_term, "Estimate"),
    p_wt           = getc("wt", "Pr(>|t|)"),
    p_hp           = getc(hp_term, "Pr(>|t|)"),
    resid_sum      = sum(residuals(fit)),
    mean_mpg       = mean(d$mpg)
  )
}

# --- Phase 5: cross-tool comparison ------------------------------------------
compare_tools <- function(r, atol = 1e-6, rtol = 1e-6) {
  rows <- lapply(STAT_ORDER, function(k) {
    py <- python_ref[[k]]
    rv <- r[[k]]
    if (is.null(rv) || is.na(rv)) {
      data.frame(stat = k, python = py, r = NA_real_, delta = NA_real_,
                 match = FALSE, note = "missing in R", stringsAsFactors = FALSE)
    } else {
      d <- abs(py - rv)
      data.frame(stat = k, python = py, r = rv, delta = d,
                 match = d <= atol + rtol * abs(py), note = "", stringsAsFactors = FALSE)
    }
  })
  do.call(rbind, rows)
}

# --- Phase 3: internal consistency checks on the R results -------------------
consistency_checks <- function(r) {
  chk <- function(desc, pass) data.frame(check = desc, pass = isTRUE(pass), stringsAsFactors = FALSE)
  do.call(rbind, list(
    chk("n_obs equals 32",            r$n_obs == 32),
    chk("model_r2 in [0, 1]",         !is.na(r$model_r2) && r$model_r2 >= 0 && r$model_r2 <= 1),
    chk("p_wt in [0, 1]",             !is.na(r$p_wt) && r$p_wt >= 0 && r$p_wt <= 1),
    chk("p_hp in [0, 1]",             is.na(r$p_hp) || (r$p_hp >= 0 && r$p_hp <= 1)),
    chk("coef_wt < 0 (expected sign)", !is.na(r$coef_wt) && r$coef_wt < 0),
    chk("resid_sum ~ 0",              !is.na(r$resid_sum) && abs(r$resid_sum) <= 1e-6)
  ))
}

# --- Phase 4: reproducibility (re-run, require identical) ---------------------
reproducible <- function(mode) {
  a <- unlist(r_results(mode))
  b <- unlist(r_results(mode))
  all(mapply(function(x, y) (is.na(x) && is.na(y)) || isTRUE(x == y), a, b))
}

# --- Overall verdict ---------------------------------------------------------
overall_pass <- function(cmp, cons, repro) {
  all(cmp$match) && all(cons$pass) && isTRUE(repro)
}

# Verdict summary used by the demo UI (banner + sidebar indicator).
verdict_of <- function(s) list(
  pass    = overall_pass(s$cmp, s$cons, s$repro),
  matched = sum(s$cmp$match),
  total   = nrow(s$cmp)
)
