library(mlogit)

data <- database %>%
    select(
        B02_1,
        B05,
        B08,
        whitealone,
        num_flights_lastyear,
        mode,
        starts_with("PA")
    ) %>%
    mutate(
        B02_1 = factor(B02_1),
        B05 = factor(B05),
        B08 = factor(B08),
        whitealone = factor(whitealone)
    ) %>%
    mutate(
        mode = factor(mode),
        num_flights_lastyear = log(num_flights_lastyear + 1)
    ) %>%
    dfidx(
        shape = "wide",
        choice = "mode"
    )

model1 <- mlogit(
    mode ~ 0 |
        B02_1 +
            B05 +
            B08 +
            num_flights_lastyear +
            whitealone +
            PA1 +
            PA2 +
            PA3 +
            PA4 +
            PA5,
    data,
    nest = list(PV = c("PV", "DP"), PT = c("PT"), RH = c("RH")),
    reflevel = "PV",
    un.nest.el = TRUE
)
summary(model1)

(coef(model1)["iv"] - 1) / sqrt(vcov(model1)["iv", "iv"])

model2 <- update(model1, nests = NULL)
summary(model2)
lrtest(model1, model2)

model3 <- update(model2, alt.subset = c("PV", "RH", "PT"))
summary(model3)

hmftest(model2, model3)
