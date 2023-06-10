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
  select(ResponseId, B02_1, starts_with("B03"), -starts_with("TEXT"), B05, B07, B08, starts_with("C00_2"), mode) %>%
  mutate(
    B02_1 = 2023 - (B02_1 %>% as.numeric()),
    whitealone = case_when(
      B03_4 == 1 & B03_1 + B03_2 + B03_3 + B03_4 + B03_5 == 1 ~ 1,
      T ~ 0
    ),
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
    num_flights_lastyear = pmax(C00_2_1_1, 0) + pmax(C00_2_1_2, 0) + pmax(C00_2_2_1, 0) + pmax(C00_2_2_2, 0),
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
  ) %>%
  select(-starts_with("B03")) %>%
  filter(mode != "PT")

apollo_initialise()

apollo_control <- list(
  modelName       = "nl_test_without_pt",
  modelDescr      = "test nl model",
  indivID         = "ResponseId",
  outputDirectory = "output"
)

apollo_beta <- c(
  asc_rh = 0,
  asc_dp = 0,
  b_inc_mid_rh = 0,
  b_inc_mid_dp = 0,
  b_inc_high_rh = 0,
  b_inc_high_dp = 0,
  b_nonwoman_rh = 0,
  b_nonwoman_dp = 0,
  b_nonwhitealone_rh = 0,
  b_nonwhitealone_dp = 0,
  b_age_mid_rh = 0,
  b_age_mid_dp = 0,
  b_age_high_rh = 0,
  b_age_high_dp = 0,
  b_num_flight_lastyear_rh = 0,
  b_num_flight_lastyear_dp = 0,
  lambda_PV = 0.5
)

apollo_fixed <- c()

apollo_inputs <- apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  P <- list()

  V <- list()
  V[["rh"]] <- asc_rh +
    b_inc_mid_rh * (B08 == 2) +
    b_inc_high_rh * (B08 == 3) +
    b_nonwoman_rh * (B05 == 2) +
    b_nonwhitealone_rh * (whitealone == 0) +
    b_age_mid_rh * (B02_1 == 2) +
    b_age_high_rh * (B02_1 == 3) +
    b_num_flight_lastyear_rh * log(num_flights_lastyear + 1)
  V[["pv"]] <- 0
  V[["dp"]] <- asc_dp +
    b_inc_mid_dp * (B08 == 2) +
    b_inc_high_dp * (B08 == 3) +
    b_nonwoman_dp * (B05 == 2) +
    b_nonwhitealone_dp * (whitealone == 0) +
    b_age_mid_dp * (B02_1 == 2) +
    b_age_high_dp * (B02_1 == 3) +
    b_num_flight_lastyear_dp * log(num_flights_lastyear + 1)

  nlNests <- list(root = 1, PV = lambda_PV)
  nlStructure <- list()
  nlStructure[["root"]] <- c("PV", "rh")
  nlStructure[["PV"]] <- c("pv", "dp")

  nl_settings <- list(
    alternatives = c(pv = 1, rh = 3, dp = 4),
    avail = list(pv = 1, rh = 1, dp = 1),
    choiceVar = mode_id,
    utilities = V,
    nlNests = nlNests,
    nlStructure = nlStructure
  )

  P[["model"]] <- apollo_nl(nl_settings, functionality)

  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(constraints = c(
    "lambda_PV < 1 + 1e-10",
    "lambda_PV > -1e-10"
  ))
)

apollo_modelOutput(model, modelOutput_settings = list(printPVal = 1))
apollo_saveOutput(model, saveOutput_settings = list(printPVal = 1))
