#' Meta-regression function for Figure 2 and Table 2
#'
#' @export
#' @param x1 Starting month to predict protection.
#' @param x2 Ending month to predict protection.
#' @param comp Comparison (takes values Infection + 1st booster dose vs naive, Infection + primary series vs naive, Prior infection vs naive)
#' @param severity Severity (takes values Any Infection, Hospitalization or severe disease)
#' @return A list with two elements (data frames):
#' Element 1 has predicted logit(protection) for plotting between x1 and x2 months.
#' Element 2 has predicted logit(protection) at intervals of 1 month between x1 and x2 months.
#'
#'
meta_reg <- function(x1,x2,comp,severity){

  xs <- seq(x1,x2,length=500)
  months <- seq(x1,x2,by=1)
  df <- reg_data %>%
    filter(variant_gisaid=="Omicron",
           months<21,
           comparison_label==comp,
           severity_of_infection==severity,
           !is.na(vi),!is.nan(vi),!is.infinite(vi))
  n_studies <- length(unique(df$he_studies))
  n_est <- nrow(df)

  ### model
  res <- rma.mv(yi ~ months, vi,
                random = ~ 1 | he_studies,
                data=df
  )

  res_pred1 <- data.frame(predict(res, newmods=xs))
  res_pred1$xs <- xs
  res_pred1$severity_of_infection <- severity
  res_pred1$variant_gisaid <- "Omicron"
  res_pred1$comparison_label <- comp

  res_pred2 <- data.frame(predict(res, newmods=months))
  res_pred2$xs <- months
  res_pred2$severity_of_infection <- severity
  res_pred2$variant_gisaid <- "Omicron"
  res_pred2$comparison_label <- comp
  res_pred2$n_studies <- n_studies
  res_pred2$n_est <- n_est

  return(list(res_pred1,res_pred2))
}
