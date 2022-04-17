test_that(".bfolds", {
    expect_error(.bfolds(1:3, nfolds = 2), ">= 3")
    expect_warning(
        .bfolds(rep(1:3, each = 2), nfolds = 5),
        "reducing to minimal group size"
    )

    set.seed(1)
    cl <- factor(rep(c("a", "b"), c(8, 4)))
    expect_equal(
        .bfolds(cl, 4),
        c(1, 4, 4, 2, 2, 3, 3, 1, 2, 3, 1, 4)
    )
})

test_that(".caseweights", {
    cl <- rep(1:2, c(2, 4))
    expect_equal(
        .caseweights(cl, replace = FALSE),
        setNames(rep(c(1000, 1), c(2, 4)), cl)
    )
    expect_equal(
        .caseweights(cl, replace = TRUE),
        setNames(rep(c(0.5, 0.25), c(2, 4)), cl)
    )
    ## fix error if classes are numerics other as 1:2
    cl <- rep(0:1, c(2, 4))
    expect_equal(
        unname(.caseweights(cl, replace = FALSE)),
        unname(.caseweights(cl + 1, replace = FALSE))
    )
})

test_that(".nmin", {
    expect_equal(.nmin(rep(1:2, c(2, 4))), 2)
    expect_equal(.nmin(rep(1:2, c(7, 4))), 4)
})

test_that(".samplefraction", {
    cl <- rep(1:2, c(2, 4))
    expect_equal(.samplefraction(cl), 2/3)
    cl <- rep(1:2, 2)
    expect_equal(.samplefraction(cl), 1)
})
