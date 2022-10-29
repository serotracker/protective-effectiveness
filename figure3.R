# Meta-regression analysis for Figure 3

df <- reg_data %>%
  filter(months<21,
         variant_gisaid=="Omicron",
         severity_of_infection=="Any Infection", #"Severe Disease or Hospitalization
         !is.na(vi),!is.nan(vi),!is.infinite(vi)) %>%
  mutate(group = case_when(comparison_label=="Infection + primary series vs naive"~"full_infection",
                           comparison_label=="Infection + 1st booster dose vs naive"~"booster_infection",
                           comparison_label=="Prior infection vs naive"~"infection",
                           comparison_label=="1st booster vaccine vs naive"~"booster",
                           comparison_label=="Full vaccine vs naive"~"full")) %>%
  mutate(months=months-6)%>%
  mutate(group=forcats::fct_relevel(group,"full_infection"))

### number studies
df %>% dplyr::select(he_studies, comparison_label) %>% distinct() %>% count(comparison_label)
### model
res <- rma.mv(yi,vi,mods=~months*group-1,
              random = ~ group | he_studies,
              struct="DIAG",
              data=df
)

### test for differences at 6 months
summary(res)
summary(glht(res, linfct = c("groupbooster - groupfull_infection = 0")))
summary(glht(res, linfct = c("groupfull - groupfull_infection = 0")))
summary(glht(res, linfct = c("groupinfection - groupfull_infection = 0")))
summary(glht(res, linfct = c("groupbooster_infection - groupfull_infection = 0")))
