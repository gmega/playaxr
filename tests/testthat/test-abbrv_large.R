
run_comparison <- function(x) {
  x_num <- unlist(x[c(TRUE, FALSE)])
  x_str <- unlist(x[c(FALSE, TRUE)])

  expect_equal(abbrv_large(x_num), x_str)
}

test_that('abbreviating formatter works', {
  x <- list(
    3e12, '3T',
    3e9, '3B',
    -3e9, '-3B',
    1e9, '1B',
    9e8, '900M',
    13234956, '13M',
    248576, '248.576k',
    -248577, '-248.577k',
    1234, '1.234k', # 3 decimals as rounding gets applied per-class
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

  run_comparison(x)
})

test_that('same-scale numbers get truncated to the right decimal place', {
  x <- c(3.5e9, 3.2e9, 2.97e6, 1.2e3, 1e3)
  expect_equal(abbrv_large(x), c('3.5B', '3.2B', '2M', '1.2k', '1k'))
})

test_that('diff-based rounding works', {
  expect_equal(
    abbrv_large(c(125234, 126357)), c('125k', '126k')
  )

  expect_equal(
    abbrv_large(c(125234, 125357)), c('125.2k', '125.3k')
  )

  expect_equal(
    abbrv_large(c(125234, 125257)), c('125.23k', '125.25k')
  )
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
