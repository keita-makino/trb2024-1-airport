library(psych)
library(GPArotation)

data <- raw_data %>%
  select(
    ResponseId,
    starts_with("A0"),
    -A02_4,
    gc
  ) %>%
  filter(
    gc == 1
  ) %>%
  select(-gc)

get_factor <- function(n) {
  is_done <- F
  rows <- 1:17

  while (!is_done) {
    data_temp <- data[, c(1 + rows)]
    factor <- data_temp %>%
      fa(
        nfactors = n,
        rotate = "cluster",
        scores = "tenBerge",
        max.iter = 2000,
        warnings = TRUE,
        SMC = TRUE,
        fm = "pa"
      )

    factor_score <- data.frame(
      psych::factor.scores(
        data_temp, factor
      )$scores
    )

    check_significance <- factor$loadings[] %>%
      tibble() %>%
      rowwise() %>%
      summarize(
        n = any(abs(.) > 0.3)
      ) %>%
      deframe()


    if (all(check_significance)) {
      is_done <- T
    }

    rows <- check_significance %>% which()
  }

  write.csv(
    factor$loadings[],
    paste0("./output/factor_", n, ".csv")
  )

  assign(
    paste0("factor_", n),
    bind_cols(
      data %>% select(ResponseId),
      factor_score
    ),
    envir = .GlobalEnv
  )
}

get_factor(5)
