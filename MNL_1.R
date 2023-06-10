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
  b_nonwoman_pt = 0,
  b_nonwoman_rh = 0,
  b_nonwoman_dp = 0,
  b_nonwhitealone_pt = 0,
  b_nonwhitealone_rh = 0,
  b_nonwhitealone_dp = 0,
  b_age_mid_pt = 0,
  b_age_mid_rh = 0,
  b_age_mid_dp = 0,
  b_age_high_pt = 0,
  b_age_high_rh = 0,
  b_age_high_dp = 0,
  b_num_flight_lastyear_pt = 0,
  b_num_flight_lastyear_rh = 0,
  b_num_flight_lastyear_dp = 0
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
    b_nonwoman_pt * (B05 == 2) +
    b_nonwhitealone_pt * (whitealone == 0) +
    b_age_mid_pt * (B02_1 == 2) +
    b_age_high_pt * (B02_1 == 3) +
    b_num_flight_lastyear_pt * log(num_flights_lastyear + 1)
  V[["rh"]] <- asc_rh +
    b_inc_mid_rh * (B08 == 2) +
    b_inc_high_rh * (B08 == 3) +
    b_nonwoman_rh * (B05 == 2) +
    b_nonwhitealone_rh * (whitealone == 0) +
    b_age_mid_rh * (B02_1 == 2) +
    b_age_high_rh * (B02_1 == 3) +
    b_num_flight_lastyear_rh * log(num_flights_lastyear + 1)
  V[["dp"]] <- asc_dp +
    b_inc_mid_dp * (B08 == 2) +
    b_inc_high_dp * (B08 == 3) +
    b_nonwoman_dp * (B05 == 2) +
    b_nonwhitealone_dp * (whitealone == 0) +
    b_age_mid_dp * (B02_1 == 2) +
    b_age_high_dp * (B02_1 == 3) +
    b_num_flight_lastyear_dp * log(num_flights_lastyear + 1)

  mnl_settings <- list(
    alternatives = c(pv = 1, pt = 2, rh = 3, dp = 4),
    avail = list(pv = 1, pt = 1, rh = 1, dp = 1),
    choiceVar = mode_id,
    utilities = V
  )

  P[["model"]] <- apollo_mnl(mnl_settings, functionality)

  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model, modelOutput_settings = list(printPVal = 1))
apollo_saveOutput(model, saveOutput_settings = list(printPVal = 1))
