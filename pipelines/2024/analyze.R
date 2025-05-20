
## See notes in study_design.R

library(data.table)

.args <- if (interactive()) c(
  "data/study_0.7_0.75.rds",
  "data/analysis_0.7_0.75.rds"
) else commandArgs(trailingOnly = TRUE)

study_dt <- readRDS(.args[1])
res_dt <- study_dt[, {
	fit <- glm(suppressed ~ treatment, family = "binomial", data = .SD)
	list(est = coef(fit)[["treatmentnew"]]
		, lwr = confint(fit)[["treatmentnew", 1]]
		, upr = confint(fit)[["treatmentnew", 2]]
	)
}, by = study_id]

saveRDS(res_dt, tail(.args, 1))