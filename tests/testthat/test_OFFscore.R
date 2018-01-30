library(ABPS)
context("OFF-score function")

testvector <- c(14.6, 0.48)
expresult <- 104.40

# OFF score is used with two decimals, but rounding of initial values
# can easily yield a 0.05 difference between two calculations. We allow
# for a 0.5 precision, which is more than enough given the scale of
# the score.
off_score_tol <- 0.5

test_that("OFFscore works on a single data point, named parameters", {
    expect_equal(OFFscore(RETP=testvector[2], HGB=testvector[1]), expresult,
                          tolerance=off_score_tol)
})

# The "blooddoping" dataset already provides OFF score values
# The test recalculates them, and makes sure that the largest absolute
# difference among all samples is small enough
dopingtest <- blooddoping

test_that("OFFscore works on the blood doping dataset", {
    expect_equal(max(abs(dopingtest$OFFscore -
                         OFFscore(HGB=dopingtest$HGB, RETP=dopingtest$RETP))),
                 0, tolerance=off_score_tol)
})

# The "bloodcontrol" dataset already provides OFF score values
# The test recalculates them, and makes sure that the largest absolute
# difference among all samples is small enough
test_that("OFFscore works on the blood control dataset", {
    expect_equal(max(abs(bloodcontrol$OFFscore -
                         OFFscore(HGB=bloodcontrol$HGB, RETP=bloodcontrol$RETP))),
                 0, tolerance=off_score_tol)
})

# Similar test, but this time we pass the entire data frame
test_that("OFFscore works on the blood control dataset passed as a data frame", {
    expect_equal(max(abs(bloodcontrol$OFFscore -
                         OFFscore(bloodcontrol))),
                 0, tolerance=off_score_tol)
})


# Similar test, but this time we pass the entire data frame
test_that("OFFscore works on the blood control dataset passed as a data frame", {
    expect_error(OFFscore(bloodcontrol,
                          HGB=bloodcontrol$HGB,
                          RETP=bloodcontrol$RETP))
})



# Our OFFscore function requires HGB in g/dL (in order to ensure coherency
# within the package), but the original publication was specifying g/L. The
# function tries to identify when there is an error of units, and should
# print a warning if HGB seems to be specified in g/L
# The test multiplies all HGB values by 10, in order to scale them to g/L
test_that("OFFscore prints a warning if HGB seems to use the wrong units.", {
    expect_warning(OFFscore(HGB=bloodcontrol$HGB*10, RETP=bloodcontrol$RETP))
})

