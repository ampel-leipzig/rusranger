test_that("smote", {
    x <- matrix(1:8, nrow = 4)
    expect_error(smote(x, 1), "length")
})

test_that(".smote", {
    expect_error(.smote(data.frame(a = 1:2)), "matrix")
    x <- matrix(1:8, nrow = 4)
    expect_error(.smote(x, k = 1:2), "length")
    expect_error(.smote(x, k = 1.2), "integer")
    expect_error(.smote(x, k = 1L), "2")
    expect_error(.smote(x, ndups = 1:2), "length")
    expect_error(.smote(x, ndups = 1.2), "integer")
    expect_error(.smote(x, ndups = 0L), "0")
    expect_identical(nrow(.smote(x, k = 2L, ndups = 1L)), 4L)
    expect_identical(nrow(.smote(x, k = 2L, ndups = 2L)), 8L)
})

test_that(".neighbours", {
    expect_error(.neighbours(1:10), "dist")
    d <- dist(1:9)
    expect_error(.neighbours(d, 1:2), "length")
    expect_error(.neighbours(d, 1.2), "integer")
    expect_equal(.neighbours(d, 3L), c(2:1, 1:6))
    expect_equal(.neighbours(d, 5L), c(4:1, 1:4))
})

test_that(".ndups", {
    expect_equal(.ndups(100, 20), 3)
    expect_equal(.ndups(100, 10), 8)
})

test_that(".minority", {
    expect_equal(.minority(rep(1:2, c(4, 2))), 2)
    expect_equal(.minority(rep(c("A", "B"), c(2, 5))), "A")
})
