m <- matrix(1:8, nrow = 2)

test_that("rowMedians", {
    expect_equal(rowMedians(m), c(4, 5))
})

test_that("colMedians", {
    expect_equal(colMedians(m), seq(1, 7, by = 2) + 0.5)
})
