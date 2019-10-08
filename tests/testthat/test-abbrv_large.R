test_that('abbreviating formatter works', {
  x <- list(
    3e9, '3B',
    -3e9, '-3B',
    1e9, '1B',
    9e8, '900M',
    13234956, '13M',
    248576, '248k',
    -248577, '-248k',
    1234, '1k',
    748, '748',
    21, '21',
    10, '10',
    9, '9',
    9.45, '9',
    1, '1',
    -1, '-1',
    0.5, '0',
    0, '0'
  )

  x_num <- unlist(x[c(TRUE, FALSE)])
  x_str <- unlist(x[c(FALSE, TRUE)])

  expect_equal(abbrv_large(x_num), x_str)
})

test_that('abbreviating formatter accepts NA without barfing', {
  x <- c(NA, 3e9, -3e9, 1e9, 13234956)
  expect_equal(abbrv_large(x), c(NA, '3B', '-3B', '1B', '13M'))
})

test_that('abbreviating formatter takes custom decimals', {
  x <- c(250129, 350182, 750921, 1500000, 2500000)
  expect_equal(abbrv_large(x, decimals = list(M = 1)),
               c('250k', '350k', '750k', '1.5M', '2.5M'))
})
