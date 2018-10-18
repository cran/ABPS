library(ABPS)
context("ABPS function")

# testvector contains the 7 parameters, in the same order as the
# functions expects them (HCT, HGB, MCH, MCHC, MCV, RBC, RETP),
# but without names
testvector <- c(43.2, 14.6, 31.1, 33.8, 92.1, 4.69, 0.48)
expresult <- -0.5398784

# ABPS is used with two decimals, and our test data does not provide
# more precision, so we specify the precision accordingly
abpstol <- 0.01

test_that("ABPS works on an unnamed vector", {
  expect_equal(ABPS(testvector), expresult, tolerance=abpstol)
})

test_that("ABPS works when parameters are specified individually", {
  expect_equal(ABPS(HCT=43.2, HGB=14.6, MCH=31.1, MCHC=33.8,
                    MCV=92.1, RBC=4.69, RETP=0.48),
		    expresult, tolerance=abpstol)
})

testvectors <- rbind(testvector, testvector)
expresults <- c(expresult, expresult)
test_that("ABPS works on an unnamed matrix", {
  expect_equal(ABPS( testvectors), expresults, tolerance=abpstol)
})

# The "bloodcontrol" dataset already provides ABPS value
# The test recalculates them, and makes sure that the largest absolute
# difference among all samples is small enough
test_that("Expected ABPS results on the blood control data", {
    expect_equal(max( abs(bloodcontrol$ABPS- ABPS(bloodcontrol)) ), 0,
                 tolerance=abpstol)
})

# The "blooddoping" dataset already provides ABPS value
# The test recalculates them, and makes sure that the largest absolute
# difference among all samples is small enough

dopingABPS <- ABPS(blooddoping)

# The first two data points contain NA, so their ABPS score should be NA too
test_that("ABPS scores calculated on NA values should yield NA", {
    expect_true(all(is.na(dopingABPS[1:2])))
})

# The other scores should yield the correct ABPS value

# The ABPS calculated in the file are slightly different from those we
# obtain with the current version of the software (as they are older,
# it is likely that the software version was different, and hence the ouptut
# too), so we use a tolerance of 0.1. However, as indicated by WADA, we are
# still within the tolerance acceptable for the ABPS.
test_that("Expected ABPS results on the blood doping data", {
    expect_equal(max(abs(blooddoping$ABPS[-(1:4)]-dopingABPS[-(1:4)])), 0,
                 tolerance=0.1)
})

# If some parameters are outside the range used by ABPS, a warning should
# be printed. Here, we test if HGB is above the range (12.9-18.2.
test_that("ABPS throws a warning when HGB is above the known range.", {
  expect_warning(ABPS(HCT=43.2, HGB=18.21, MCH=31.1, MCHC=33.8,
                      MCV=92.1, RBC=4.69, RETP=0.48))
})
