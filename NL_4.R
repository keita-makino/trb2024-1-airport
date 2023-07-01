apollo_initialise()

apollo_control <- list(
  modelName       = "nl_4",
  modelDescr      = "nl model with factor scores and opinions",
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
  b_num_flight_lastyear_dp = 0,
  b_factor1_pt = 0,
  b_factor1_rh = 0,
  b_factor1_dp = 0,
  b_factor2_pt = 0,
  b_factor2_rh = 0,
  b_factor2_dp = 0,
  b_factor3_pt = 0,
  b_factor3_rh = 0,
  b_factor3_dp = 0,
  b_factor4_pt = 0,
  b_factor4_rh = 0,
  b_factor4_dp = 0,
  b_factor5_pt = 0,
  b_factor5_rh = 0,
  b_factor5_dp = 0,
  b_availability_pv = 0,
  b_availability_pt = 0,
  b_availability_rh = 0,
  b_easeofuse_pv = 0,
  b_easeofuse_pt = 0,
  b_easeofuse_rh = 0,
  b_accessibility_pv = 0,
  b_accessibility_home_pt = 0,
  b_accessibility_station_pt = 0,
  b_accessibility_rh = 0,
  b_affordability_pv = 0,
  b_affordability_pt = 0,
  b_cleanliness_pv = 0,
  b_cleanliness_pt = 0,
  b_cleanliness_vehicle_rh = 0,
  b_cleanliness_location_rh = 0,
  lambda_NP = 0.5
)

apollo_fixed <- c()

apollo_inputs <- apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  P <- list()

  V <- list()
  V[["pv"]] <- b_availability_pv * av_pv * D01_1_1 +
    b_easeofuse_pv * av_pv * D01_1_2 +
    b_accessibility_pv * av_pv * D01_1_3 +
    b_affordability_pv * av_pv * D01_1_4 +
    b_cleanliness_pv * av_pv * D01_1_5
  V[["pt"]] <- asc_pt +
    b_inc_mid_pt * (B08 == 2) +
    b_inc_high_pt * (B08 == 3) +
    b_nonwoman_pt * (B05 == 2) +
    b_nonwhitealone_pt * (whitealone == 0) +
    b_age_mid_pt * (B02_1 == 2) +
    b_age_high_pt * (B02_1 == 3) +
    b_num_flight_lastyear_pt * log(num_flights_lastyear + 1) +
    b_factor1_pt * PA1 +
    b_factor2_pt * PA2 +
    b_factor3_pt * PA3 +
    b_factor4_pt * PA4 +
    b_factor5_pt * PA5 +
    b_availability_pt * av_bu * D01_2_1 +
    b_easeofuse_pt * av_bu * D01_2_2 +
    b_accessibility_station_pt * av_bu * D01_2_3 +
    b_accessibility_home_pt * av_bu * D01_2_4 +
    b_affordability_pt * av_bu * D01_2_5 +
    b_cleanliness_pt * av_bu * D01_2_6 +
    b_availability_pt * av_ra * D01_3_1 +
    b_easeofuse_pt * av_ra * D01_3_2 +
    b_accessibility_station_pt * av_ra * D01_3_3 +
    b_accessibility_home_pt * av_ra * D01_3_4 +
    b_affordability_pt * av_ra * D01_3_5 +
    b_cleanliness_pt * av_ra * D01_3_6
  V[["rh"]] <- asc_rh +
    b_inc_mid_rh * (B08 == 2) +
    b_inc_high_rh * (B08 == 3) +
    b_nonwoman_rh * (B05 == 2) +
    b_nonwhitealone_rh * (whitealone == 0) +
    b_age_mid_rh * (B02_1 == 2) +
    b_age_high_rh * (B02_1 == 3) +
    b_num_flight_lastyear_rh * log(num_flights_lastyear + 1) +
    b_factor1_rh * PA1 +
    b_factor2_rh * PA2 +
    b_factor3_rh * PA3 +
    b_factor4_rh * PA4 +
    b_factor5_rh * PA5 +
    b_availability_rh * av_rh * D01_4_1 +
    b_easeofuse_rh * av_rh * D01_4_2 +
    b_accessibility_rh * av_rh * D01_4_3 +
    b_cleanliness_vehicle_rh * av_rh * D01_4_5 +
    b_cleanliness_location_rh * av_rh * D01_4_6
  V[["dp"]] <- asc_dp +
    b_inc_mid_dp * (B08 == 2) +
    b_inc_high_dp * (B08 == 3) +
    b_nonwoman_dp * (B05 == 2) +
    b_nonwhitealone_dp * (whitealone == 0) +
    b_age_mid_dp * (B02_1 == 2) +
    b_age_high_dp * (B02_1 == 3) +
    b_num_flight_lastyear_dp * log(num_flights_lastyear + 1) +
    b_factor1_dp * PA1 +
    b_factor2_dp * PA2 +
    b_factor3_dp * PA3 +
    b_factor4_dp * PA4 +
    b_factor5_dp * PA5


  nlNests <- list(root = 1, NP = lambda_NP)
  nlStructure <- list()
  nlStructure[["root"]] <- c("pv", "NP")
  nlStructure[["NP"]] <- c("dp", "rh", "pt")

  nl_settings <- list(
    alternatives = c(pv = 1, pt = 2, rh = 3, dp = 4),
    avail = list(pv = av_pv, pt = av_pt, rh = av_rh, dp = av_dp),
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
    "lambda_NP < 1 + 1e-10",
    "lambda_NP > -1e-10"
  ))
)

apollo_modelOutput(model, modelOutput_settings = list(printPVal = 1))
apollo_saveOutput(model, saveOutput_settings = list(printPVal = 1))

apollo_lrTest("./output/mnl_4", model)
