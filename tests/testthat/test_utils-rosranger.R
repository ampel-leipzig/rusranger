test_that(".caseweights", {
    cl <- rep(1:2, c(2, 4))
    expect_equal(
        .roscaseweights(cl, 2), rep(c(5, 1), c(2, 4))
    )
    expect_equal(
        .roscaseweights(cl, 4), rep(c(7, 1), c(2, 4))
    )
})
