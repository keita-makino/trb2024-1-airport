rm(list = ls())

library(tidyverse)
library(haven)
library(apollo)

raw_data <- read_sav(file.path("data", "data.sav"))

database <- raw_data %>%
  filter(gc == 1) %>%
  filter(C02_1 < 5) %>%
  mutate(
    mode = case_when(
      C02_11_6 == 1 | C02_11_7 == 1 ~ "PT",
      C02_11_4 == 1 | C02_11_5 == 1 | C02_11_8 == 1 | C02_11_9 == 1 ~ "RH",
      C02_11_1 == 1 | C02_11_2 == 1 ~ "PV",
      C02_11_3 == 1 ~ "DP",
      T ~ "OT"
    )
  ) %>%
  mutate(
    mode = case_when(
      ResponseId == "R_0vNEJKFm1cccxR7" |
        ResponseId == "R_BsRKjKlUHLyeHGp" ~ "PT",
      ResponseId == "R_2VeHTu5jM3yAli4" |
        ResponseId == "R_1jKefwYhDtLLdiI" |
        ResponseId == "R_DGFpvNEoJq4kTct" |
        ResponseId == "R_prAMdgIL0Jq8c8x" |
        ResponseId == "R_3fjwhWrBC8H6ziG" ~ "RH",
      ResponseId == "R_1fdeOgoVbX3nRZR" |
        ResponseId == "R_22RMDzkhcPb0N8T" |
        ResponseId == "R_1qako2y1vHpvJWV" |
        ResponseId == "R_ZpNu06EbZ9fz53X" |
        ResponseId == "R_1eyKP8FAKPBAZTl" |
        ResponseId == "R_sgN9nR14E1UnEWJ" |
        ResponseId == "R_2TT4jWXJ5XXM422" |
        ResponseId == "R_1QFLAy8xLGXPLbB" |
        ResponseId == "R_1OTUzpkMGaYnkOw" |
        ResponseId == "R_1HhM7uQTsbE6gBO" |
        ResponseId == "R_XhA2BfL2xeMR9PH" ~ "DP",
      ResponseId == "R_2sRpuS5mc2ac4Nf" |
        ResponseId == "R_vHyGRooZE57D25H" |
        ResponseId == "R_2R7y3JD6c4Dzwvj" |
        ResponseId == "R_2PvgfEGneJBJC78" ~ "PV",
      T ~ mode
    )
  ) %>%
  select(ResponseId, B02_1, B05, B07, B08, mode) %>%
  mutate(
    B02_1 = 2023 - (B02_1 %>% as.numeric()),
    B05 = case_when(
      B05 == 1 ~ 1,
      T ~ 2
    ),
    B07 = case_when(
      B07 < 3 ~ 1,
      B07 < 5 ~ 2,
      T ~ 3
    ),
    B08 = case_when(
      B08 < 3 ~ 1,
      B08 < 6 ~ 2,
      T ~ 3
    ),
    mode_id = case_when(
      mode == "PV" ~ 1,
      mode == "PT" ~ 2,
      mode == "RH" ~ 3,
      mode == "DP" ~ 4
    )
  ) %>%
  mutate(
    B02_1 = case_when(
      B02_1 < 35 ~ 1,
      B02_1 < 55 ~ 2,
      T ~ 3
    )
  )

apollo_initialise()

apollo_control <- list(
  modelName       = "mnl_test",
  modelDescr      = "test mnl model",
  indivID         = "ResponseId",
  outputDirectory = "output"
)

apollo_beta <- c(
  asc_pt = 0,
  asc_rh = 0,
  asc_dp = 0,
  b_inc_mid_pt = 0,
  b_inc_mid_rh = 0,
  b_inc_mid_dp = 0,
  b_inc_high_pt = 0,
  b_inc_high_rh = 0,
  b_inc_high_dp = 0,
  b_gen_nonwoman_pt = 0,
  b_gen_nonwoman_rh = 0,
  b_gen_nonwoman_dp = 0,
  b_age_mid_pt = 0,
  b_age_mid_rh = 0,
  b_age_mid_dp = 0,
  b_age_high_pt = 0,
  b_age_high_rh = 0,
  b_age_high_dp = 0
)

apollo_fixed <- c()

apollo_inputs <- apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  P <- list()

  V <- list()
  V[["pv"]] <- 0
  V[["pt"]] <- asc_pt +
    b_inc_mid_pt * (B08 == 2) +
    b_inc_high_pt * (B08 == 3) +
    b_gen_nonwoman_pt * (B05 == 2) +
    b_age_mid_pt * (B02_1 == 2) +
    b_age_high_pt * (B02_1 == 3)
  V[["rh"]] <- asc_rh +
    b_inc_mid_rh * (B08 == 2) +
    b_inc_high_rh * (B08 == 3) +
    b_gen_nonwoman_rh * (B05 == 2) +
    b_age_mid_rh * (B02_1 == 2) +
    b_age_high_rh * (B02_1 == 3)
  V[["dp"]] <- asc_dp +
    b_inc_mid_dp * (B08 == 2) +
    b_inc_high_dp * (B08 == 3) +
    b_gen_nonwoman_dp * (B05 == 2) +
    b_age_mid_dp * (B02_1 == 2) +
    b_age_high_dp * (B02_1 == 3)

  mnl_settings <- list(
    alternatives = c(pv = 1, pt = 2, rh = 3, dp = 4),
    avail = list(pv = 1, pt = 1, rh = 1, dp = 1),
    choiceVar = mode_id,
    explanators = c("B08"),
    utilities = V
  )

  P[["model"]] <- apollo_mnl(mnl_settings, functionality)

  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model)
apollo_saveOutput(model)
