library(mlogit)

data <- database %>%
    select(
        B02_1, B05, B07, B08, num_flights_lastyear, mode
    ) %>%
    mutate(
        mode = factor(mode),
        B02_1.PV = B02_1,
        B02_1.PT = B02_1,
        B02_1.RH = B02_1,
        B02_1.DP = B02_1,
        num_flights_lastyear = log(num_flights_lastyear + 1)
    ) %>%
    select(
        -B02_1
    ) %>%
    dfidx(
        varying = 6:9,
        choice = "mode"
    )

model1 <- mlogit(
    mode ~ 0 | B02_1 + B05 + B08 + num_flights_lastyear,
    data,
    nest = list("P" = c("PV", "DP"), "O" = c("PT", "RH")),
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
