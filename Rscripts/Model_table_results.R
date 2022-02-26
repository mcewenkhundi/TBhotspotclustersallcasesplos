library(tidyverse)
library(brms)


model_parameters_notif_rintc <- function(model_notif_rintc, model_name) {
  #model_notif_rintc <- model_parameters(model_notif_rintc, centrality = "mean", effects = "all", component = "all", ci = 0.95, exponentiate = TRUE)
  model_notif_rintc <- posterior_summary(model_notif_rintc)

  model_notif_rintc.parameters <- row.names(model_notif_rintc)
  model_notif_rintc <- as_tibble(model_notif_rintc)

  model_notif_rintc <- model_notif_rintc %>%
    mutate(Parameter = model_notif_rintc.parameters, .before = 1) %>%
    select(Parameter, Mean=Estimate, CI_low=Q2.5, CI_high=Q97.5)

  model_notif_rintc1 <- model_notif_rintc %>%
    filter(!str_detect(Parameter, "sd_cluster__Intercept|sdcar|zi")) %>%
    filter(!str_detect(Parameter, "rrintc|lp__|r_cluster")) %>%
    mutate(across(c(Mean, CI_low, CI_high), ~exp(.x)))

  model_notif_rintc2 <- model_notif_rintc %>%
    filter(str_detect(Parameter, "sd_cluster__Intercept|sdcar|zi"))

  model_notif_rintc <- bind_rows(model_notif_rintc1, model_notif_rintc2)


  model_notif_rintc$Mean[1] <- model_notif_rintc$Mean[1] * 100000
  model_notif_rintc$CI_low[1] <- model_notif_rintc$CI_low[1] * 100000
  model_notif_rintc$CI_high[1] <- model_notif_rintc$CI_high[1] * 100000

  # model_name <-  deparse(substitute(model_notif_rintc)) #https://stackoverflow.com/questions/10520772/in-r-how-to-get-an-objects-name-after-it-is-sent-to-a-function
  # model_name <- paste0(model_name, "m")

  model_notif_rintc %>%
    select(Parameter, Mean, CI_low, CI_high) %>%
    mutate(across(where(is.numeric), ~ formatC(round(.x, 2), 2, format = "f"))) %>%
    mutate(Parameter = str_replace(Parameter, "b_", "")) %>%
    mutate( {{model_name}} := paste0(Mean, " (", CI_low, "-", CI_high, ")")) %>% #https://stackoverflow.com/questions/26003574/use-dynamic-variable-names-in-dplyr
    select(Parameter, {{model_name}})
}

notif_model_rintc_25 <- readRDS(here::here("data/notif_model_rintc_25.rds"))

model_parameters_notif_rintc(notif_model_rintc_25, "notif_model_rintc_25") %>%
  gt::gt() %>%
  gt::fmt_missing(
    everything(),
    missing_text = ""
  ) %>%
  gt::gtsave(filename = "S8_Table_allnotified_models_rintc.rtf", path = here::here("figures"))
