rm(list = ls())

library(tidyverse)
library(haven)
library(apollo)

raw_data <- read_sav(file.path("data", "data.sav"))

access_mode <- read.csv(file.path("data", "accessmode.csv"), header = TRUE)

database <- raw_data %>%
  inner_join(access_mode, by = "ResponseId") %>%
  select(ResponseId, B08, mode) %>%
  mutate(
    B08 = B08 %>% as.numeric(),
    mode_id = case_when(
      mode == "PV" ~ 1,
      mode == "PT" ~ 2,
      mode == "RH" ~ 3,
      mode == "OT" ~ 4
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
  asc_pv = 0,
  asc_pt = 0,
  asc_rh = 0,
  asc_ot = 0,
  b_inc_pv = 0,
  b_inc_pt = 0,
  b_inc_rh = 0,
  b_inc_ot = 0
)

apollo_fixed <- c("asc_pv")

apollo_inputs <- apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  P <- list()

  V <- list()
  V[["pv"]] <- asc_pv + b_inc_pv * B08
  V[["pt"]] <- asc_pt + b_inc_pt * B08
  V[["rh"]] <- asc_rh + b_inc_rh * B08
  V[["ot"]] <- asc_ot + b_inc_ot * B08

  mnl_settings <- list(
    alternatives = c(pv = 1, pt = 2, rh = 3, ot = 4),
    avail = list(pv = 1, pt = 1, rh = 1, ot = 1),
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
