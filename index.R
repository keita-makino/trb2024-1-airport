rm(list = ls())

library(tidyverse)
library(haven)
library(apollo)

raw_data <- read_sav(file.path("data", "data.sav"))

source("FA.R")

database <- raw_data %>%
  filter(gc == 1) %>%
  inner_join(factor_5, by = "ResponseId") %>%
  filter(C02_1 < 5) %>%
  mutate(
    mode = case_when(
      C02_11_6 == 1 ~ "BU",
      C02_11_7 == 1 ~ "RA",
      C02_11_4 == 1 | C02_11_5 == 1 | C02_11_8 == 1 | C02_11_9 == 1 ~ "RH",
      C02_11_1 == 1 | C02_11_2 == 1 ~ "PV",
      C02_11_3 == 1 ~ "DP",
      T ~ "OT"
    )
  ) %>%
  mutate(
    mode = case_when(
      ResponseId == "R_0vNEJKFm1cccxR7" |
        ResponseId == "R_BsRKjKlUHLyeHGp" ~ "BU",
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
      mode == "BU" ~ 2,
      mode == "RA" ~ 2,
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
  mutate(
    av_pv = case_when(
      mode == "PV" | C02_16_1 >= 3 | C02_16_2 >= 3 ~ 1,
      T ~ 0
    ),
    av_bu = case_when(
      mode == "BU" | C02_16_6 >= 3 ~ 1,
      T ~ 0
    ),
    av_ra = case_when(
      C02_1 == 3 ~ 0,
      mode == "RA" | C02_16_7 >= 3 ~ 1,
      T ~ 0
    ),
    av_pt = case_when(
      mode == "BU" | mode == "RA" | C02_16_6 >= 3 | C02_16_7 >= 3 ~ 1,
      T ~ 0
    ),
    av_rh = case_when(
      mode == "RH" | C02_16_4 >= 3 | C02_16_5 >= 3 | C02_16_8 >= 3 | C02_16_9 >= 3 ~ 1,
      T ~ 0
    ),
    av_dp = case_when(
      mode == "DP" | C02_16_3 >= 3 ~ 1,
      T ~ 0
    ),
    B15 = B15_01 + B15_02
  ) %>%
  mutate_at(
    vars(starts_with("D01")),
    ~ {
      y <- (.x %>% replace(. == 6, NA) - 3)
    }
  ) %>%
  select(
    starts_with("B15"),
    ResponseId,
    C02_1,
    C02_12_2,
    B02_1,
    -starts_with("TEXT"),
    B05,
    B07,
    B08,
    whitealone,
    starts_with("C00_2"),
    starts_with("PA"),
    num_flights_lastyear,
    mode,
    mode_id,
    starts_with("av"),
    starts_with("D01")
  )
