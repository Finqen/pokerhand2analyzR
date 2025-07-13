test_that("find_threat_hands returns proper structure", {
  result <- find_threat_hands("AS KH", "QS JS 7S")
  expect_s3_class(result, "data.frame")
  expect_named(result, c("rank", "threat", "probability", "description"))
  expect_type(result$threat, "character")
  expect_type(result$probability, "double")
})

test_that("empty board returns empty data frame", {
  result <- find_threat_hands("AS KH", "")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result, c("threat", "probability", "description"))
})

test_that("flush threats are detected", {
  result <- find_threat_hands("AS KH", "QS JS 7S")  # 3 spades
  expect_true(any(result$threat == "Flush"))
  flush_row <- result[result$threat == "Flush", ]
  expect_gt(flush_row$probability, 0.1)
  expect_true(grepl("suited", flush_row$description))
})

test_that("straight threats are detected", {
  result <- find_threat_hands("AS KH", "9H TC JC")  # Connected cards
  expect_true(any(result$threat == "Straight"))
  straight_row <- result[result$threat == "Straight", ]
  expect_gt(straight_row$probability, 0.05)
  expect_true(grepl("Connected", straight_row$description))
})

test_that("set threats are detected", {
  result <- find_threat_hands("AS KH", "QS QH 7C")  # Pair on board
  expect_true(any(result$threat == "Set/Full House"))
  set_row <- result[result$threat == "Set/Full House", ]
  expect_gt(set_row$probability, 0.05)
  expect_true(grepl("Paired", set_row$description))
})

test_that("overpair threats are detected", {
  result <- find_threat_hands("AS KH", "5H 3C 2S")  # Very low board
  expect_true(any(result$threat == "Overpair"))
  overpair_row <- result[result$threat == "Overpair", ]
  expect_gt(overpair_row$probability, 0.05)
  expect_true(grepl("Higher", overpair_row$description))
})

test_that("threats are sorted by probability", {
  result <- find_threat_hands("AS KH", "QS JS 7S QH")  # Multiple threats
  if (nrow(result) > 1) {
    expect_true(all(diff(result$probability) <= 0))  # Descending order
    expect_equal(result$rank, 1:nrow(result))
  }
})

test_that("opponent count affects probabilities", {
  result1 <- find_threat_hands("AS KH", "QS JS 7S", 1)
  result2 <- find_threat_hands("AS KH", "QS JS 7S", 3)

  if (nrow(result1) > 0 && nrow(result2) > 0) {
    # More opponents should generally increase threat probabilities
    expect_gte(max(result2$probability), max(result1$probability))
  }
})

test_that("calculate_flush_threat works correctly", {
  expect_equal(calculate_flush_threat(2, 1), 0)  # No threat with 2 suited
  expect_gt(calculate_flush_threat(3, 1), 0.1)   # Some threat with 3 suited
  expect_gt(calculate_flush_threat(4, 1), 0.3)   # Higher threat with 4 suited
})

test_that("calculate_straight_threat works correctly", {
  expect_equal(calculate_straight_threat(c(2, 5, 9), 1), 0)  # No connected cards
  expect_gt(calculate_straight_threat(c(7, 8, 9), 1), 0.05)  # Connected sequence
})

test_that("calculate_set_threat works correctly", {
  expect_gt(calculate_set_threat(2, 1), 0.05)  # Pair on board
  expect_gt(calculate_set_threat(3, 1), 0.2)   # Trips on board
})

test_that("calculate_overpair_threat works correctly", {
  expect_gt(calculate_overpair_threat(0, 1), 0.1)   # Very low board rank (2)
  expect_gt(calculate_overpair_threat(5, 1), 0.05)  # Medium board rank (7)
  expect_lt(calculate_overpair_threat(12, 1), 0.02) # High board rank (Ace)
})

test_that("probabilities are within reasonable bounds", {
  result <- find_threat_hands("AS KH", "QS JS 7S QH")
  expect_true(all(result$probability >= 0))
  expect_true(all(result$probability <= 1))
})
