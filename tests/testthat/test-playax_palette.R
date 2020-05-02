test_that('palette_playax returns assigned colors', {
  expect_equal(palette_playax(), PLAYAX_PALETTE[names(PLAYAX_PALETTE) != ''])
})

test_that('palette_playax_unassigned returns unassigned colors', {
  expect_equal(palette_playax_unassigned(),
               unname(PLAYAX_PALETTE[names(PLAYAX_PALETTE) == '']))
})

test_that('palette_playax_select actually selects', {
  expect_equal(
    palette_playax_select(c('youtube', 'facebook')),
    PLAYAX_PALETTE[c('youtube', 'facebook')]
  )
})

test_that('palette_playax_extended binds names to unassigned slots', {
  named <- length(palette_playax())
  extended_palette <- palette_playax_extended(extension =  c('apple', 'outros'))
  expect_equal(extended_palette[['apple']], unname(PLAYAX_PALETTE[named + 1]))
  expect_equal(extended_palette[['outros']], unname(PLAYAX_PALETTE[named + 2]))
})

test_that('palette_playax_extended barfs if extension too large', {
  expect_error(
    palette_playax_extended(extension = c('apple', 'outros', 'mamãe', 'papai')),
    'Palette has 3 available slots but extension has 4 elements.'
  )
})

test_that('renaming reassigns existing colors', {
  new_palette <- palette_playax_extended(
    replacement = c('myspace' = 'facebook', 'mytube' = 'youtube')
  )

  expect_equal(palette_playax()[['facebook']], new_palette[['myspace']])
  expect_equal(palette_playax()[['youtube']], new_palette[['mytube']])

  expect_error(new_palette[['youtube']], 'subscript out of bounds')
  expect_error(new_palette[['facebook']], 'subscript out of bounds')
})
