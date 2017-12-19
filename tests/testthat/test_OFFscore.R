library(ABPS)
context("OFF-score function")

testvector <- c(146, 0.48)
expresult <- 104.40

# OFFs core is used with two decimals, but rounding of initial values
# can easily yield a 0.05 difference between two calculations. We allow
# for a 0.5 precision, which is more than enough given the scale off
# the score.
off_score_tol <- 0.5

test_that("OFFscore works on a single data point, unnamed parameters", {
    expect_equal(OFFscore(testvector[1], testvector[2]), expresult,
                          tolerance=off_score_tol)
})


test_that("OFFscore works on a single data point, named parameters", {
    expect_equal(OFFscore(RETP=testvector[2], HGB=testvector[1]), expresult,
                          tolerance=off_score_tol)
})

# The "blooddoping" dataset already provides OFF score values
# The test recalculates them, and makes sure that the largest absolute
# difference among all samples is small enough
# Note: HGB is measured in g/l in the dataset, and should be in g/dl 
# for calculating the off-score. A factor 10 is thus present in the call.
# Note: OFF score results in row 8 and 10 of the blooddoping data seem
# incorrect in the original dataset, and are thus not tested here.
# (OFF score for row 8 is 9.17 units too small, while for row 10 it is
# 9.18 units too large -- a strange symmetry).

dopingtest <- blooddoping[ -c(8,10), ]

test_that("OFFscore works on the blood doping dataset", {
    expect_equal(max(abs(dopingtest$OFFscore -
                         OFFscore(dopingtest$HGB*10, dopingtest$RETP))),
                 0, tolerance=off_score_tol)
})

# The "bloodcontrol" dataset already provides OFF score values
# The test recalculates them, and makes sure that the largest absolute
# difference among all samples is small enough
# Note: HGB is measured in g/l in the dataset, and should be in g/dl 
# for calculating the off-score. A factor 10 is thus present in the call.
test_that("OFFscore works on the blood control dataset", {
    expect_equal(max(abs(bloodcontrol$OFFscore -
                         OFFscore(bloodcontrol$HGB*10, bloodcontrol$RETP))),
                 0, tolerance=off_score_tol)
})

# OFFscore requires HGB in g/l, while ABPS (and the datasets provided)
# use g/dl. The function tries to identify when there is an error in the
# unit (and should catch in particular an 10-fold error)
test_that("OFFscore prints a warning if HGB seems to use the wrong units.", {
    expect_warning(OFFscore(bloodcontrol$HGB, bloodcontrol$RETP))
})

