context("regsdotgov access")

test_that("basic retrievals work", {
  docs1 = documents(apikey = Sys.getenv("regsgovkey"), agency = "EPA",
                    postedDate1 = "2015-01-01", postedDate2 = "2015-02-01")
  expect_is(docs1, "data.frame")
  expect_more_than(nrow(docs1), 0)
}
)

test_that("authentication works", {
  expect_is(documents(agency = "EPA", newlyPosted = 15), "data.frame")
  expect_error(documents(agency = "EPA", newlyPosted = 15, apikey = ""))
})
