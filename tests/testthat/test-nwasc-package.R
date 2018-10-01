context("test-nwasc-package")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that(".onLoad works", {
  expect_equal(require(nwasc), TRUE)
})

test_that("datasets are data.frame", {
  expect_is(nwasc.ph1.cts.dat, "data.frame")
  expect_is(nwasc.ph1.dts.dat, "data.frame")
  expect_is(nwasc.ph1.dts.obs.dat, "data.frame")
  expect_is(nwasc.ph1.obs.pre, "data.frame")
  expect_is(nwasc.ph1.shp.pre, "data.frame")
  expect_is(nwasc.ph2.cts.dat, "data.frame")
  expect_is(nwasc.ph2.obs.pre, "data.frame")
  expect_is(nwasc.ph2.shp.pre, "data.frame")
})

test_that("SegmentCTS produces df",{
  expect_is(nwasc_segmentCTS(test_obs, test_trk, test_cts), "data.frame")
})

test_that("SegmentDTS produces df",{
  expect_is(nwasc_segmentDTS(test_dts_obs, test_dts_dat), "data.frame")
})