apollo_initialise()

apollo_control <- list(
  modelName       = "nl_2_copy",
  modelDescr      = "nl model with factor scores",
  indivID         = "ResponseId",
  outputDirectory = "output"
)

apollo_beta <- c(
  asc_pt = 0,
  asc_rh = 0,
  asc_dp = 0,
  b_num_flight_lastyear_pt = 0,
  b_num_flight_lastyear_rh = 0,
  b_num_flight_lastyear_dp = 0,
  lambda_NP = 0.5
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
    b_num_flight_lastyear_pt * log(num_flights_lastyear + 1)
  V[["rh"]] <- asc_rh +
    b_num_flight_lastyear_rh * log(num_flights_lastyear + 1)
  V[["dp"]] <- asc_dp +
    b_num_flight_lastyear_dp * log(num_flights_lastyear + 1)

  nlNests <- list(root = 1, NP = lambda_NP)
  nlStructure <- list()
  nlStructure[["root"]] <- c("pv", "NP")
  nlStructure[["NP"]] <- c("pt", "rh", "dp")

  nl_settings <- list(
    alternatives = c(pv = 1, pt = 2, rh = 3, dp = 4),
    avail = list(pv = 1, pt = 1, rh = 1, dp = 1),
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

apollo_lrTest("./output/mnl_2", model)
