#' Evaluate hand strength using compact algorithm (adapted from Python)
#'
#' @param my_hand String like "AS KH" (my hole cards)
#' @param board String like "9H TC JC QS KC" (community cards)
#' @param opponents Number of opponents (default: 1)
#'
#' @return List with hand_score, hand_type, win_estimate, and description
#' @export
#' @examples
#' evaluate_hand_strength("AS AH", "9H TC JC")
#' evaluate_hand_strength("QH QS", "9C TD JH AS KC", 2)
evaluate_hand_strength <- function(my_hand, board = "", opponents = 1) {
  # Parse and evaluate my hand
  my_cards <- parse_cards(paste(board, my_hand))
  my_eval <- evaluate_hand(my_cards)

  # Get hand description
  hand_type <- get_hand_name(my_eval$score)

  # Estimate win rate based on hand strength and opponents
  base_strength <- get_base_strength(my_eval$score)
  win_estimate <- base_strength * (0.85 ^ (opponents - 1))

  return(list(
    hand_score = my_eval$score,
    hand_type = hand_type,
    win_estimate = round(win_estimate, 3),
    description = paste(hand_type, "vs", opponents, "opponents")
  ))
}

#' Parse card string into rank indices and suits
#'
#' Converts a string of cards into a list of card objects with rank and suit components.
#' Handles standard poker notation where ranks are 2-9, T, J, Q, K, A and suits are H, D, C, S.
#'
#' @param card_string String containing cards separated by spaces (e.g., "AS KH QC")
#' @return List of card objects, each containing rank (0-12) and suit components
#' @export
#' @examples
#' parse_cards("AS KH QC")
#' parse_cards("9H TC JC QS KC")
#' parse_cards("")  # Returns empty list
parse_cards <- function(card_string) {
  if (card_string == "" || is.na(card_string)) return(list())

  ranks <- "23456789TJQKA"
  cards <- strsplit(gsub("10", "T", card_string), "\\s+")[[1]]
  cards <- cards[cards != ""]

  result <- list()
  for (i in seq_along(cards)) {
    card <- cards[i]
    if (nchar(card) >= 2) {
      rank_char <- substr(card, 1, 1)
      suit_char <- substr(card, 2, 2)
      rank_idx <- which(strsplit(ranks, "")[[1]] == rank_char)

      if (length(rank_idx) > 0) {
        # Convert to 0-based indexing and create proper list structure
        result[[length(result) + 1]] <- list(rank = rank_idx[1] - 1, suit = suit_char)
      }
    }
  }
  return(result)
}

#' Evaluate poker hand strength (adapted from Python algorithm)
#'
#' Determines the poker hand ranking for a given set of cards. Handles 5-card hands
#' directly and finds the best 5-card combination for hands with more than 5 cards.
#'
#' @param cards List of card objects with rank and suit components
#' @return List containing hand score (0-8) and sorted rank vector for tie-breaking
#' @export
#' @examples
#' cards <- parse_cards("AS KH QC JD TS")
#' evaluate_hand(cards)
#'
#' # With more than 5 cards (finds best 5-card hand)
#' cards <- parse_cards("AS AH AC AD 2S 3H 4C")
#' evaluate_hand(cards)
evaluate_hand <- function(cards) {
  if (length(cards) == 0) return(list(score = 0, ranks = c()))

  # If more than 5 cards, find best 5-card hand
  if (length(cards) > 5) {
    best_score <- 0
    best_ranks <- c()

    # Try removing each card to find best combination
    for (i in 1:length(cards)) {
      sub_hand <- cards[-i]
      result <- evaluate_hand(sub_hand)
      if (result$score > best_score ||
          (result$score == best_score && compare_ranks(result$ranks, best_ranks) > 0)) {
        best_score <- result$score
        best_ranks <- result$ranks
      }
    }
    return(list(score = best_score, ranks = best_ranks))
  }

  # Pad to 5 cards if needed (use proper list construction)
  while (length(cards) < 5) {
    cards[[length(cards) + 1]] <- list(rank = 0, suit = "X")
  }

  # Extract ranks and suits using proper accessors
  ranks <- sapply(cards, function(x) {
    if (is.list(x) && !is.null(x$rank)) {
      return(x$rank)
    } else {
      return(0)  # default value
    }
  })

  suits <- sapply(cards, function(x) {
    if (is.list(x) && !is.null(x$suit)) {
      return(x$suit)
    } else {
      return("X")  # default value
    }
  })

  rank_counts <- table(ranks)
  count_rank_pairs <- data.frame(
    count = as.numeric(rank_counts),
    rank = as.numeric(names(rank_counts))
  )
  count_rank_pairs <- count_rank_pairs[order(-count_rank_pairs$count, -count_rank_pairs$rank), ]

  score_vec <- count_rank_pairs$count
  ranks_vec <- count_rank_pairs$rank

  # Handle 5 different ranks (straight/flush potential)
  if (nrow(count_rank_pairs) == 5) {
    # Check for wheel (A-2-3-4-5)
    if (ranks_vec[1] == 12 && ranks_vec[2] == 3) {  # A and 4
      ranks_vec <- c(3, 2, 1, 0, -1)
    }

    is_flush <- length(unique(suits)) == 1
    is_straight <- (ranks_vec[1] - ranks_vec[5] == 4)

    if (is_flush && is_straight) {
      score <- 8  # Straight flush
    } else if (is_flush) {
      score <- 5  # Flush
    } else if (is_straight) {
      score <- 4  # Straight
    } else {
      score <- 0  # High card
    }
  } else {
    # Standard hand types
    if (score_vec[1] == 4) {
      score <- 7  # Four of a kind
    } else if (score_vec[1] == 3 && score_vec[2] == 2) {
      score <- 6  # Full house
    } else if (score_vec[1] == 3) {
      score <- 3  # Three of a kind
    } else if (score_vec[1] == 2 && score_vec[2] == 2) {
      score <- 2  # Two pair
    } else if (score_vec[1] == 2) {
      score <- 1  # One pair
    } else {
      score <- 0  # High card
    }
  }

  return(list(score = score, ranks = ranks_vec))
}

#' Compare two rank vectors for poker hand tie-breaking
#'
#' Compares two rank vectors to determine which represents a stronger hand.
#' Used for tie-breaking when hands have the same score.
#'
#' @param ranks1 First rank vector (numeric)
#' @param ranks2 Second rank vector (numeric)
#' @return Integer: 1 if ranks1 > ranks2, -1 if ranks1 < ranks2, 0 if equal
#' @export
#' @examples
#' compare_ranks(c(12, 11, 10), c(12, 11, 9))  # Returns 1 (first is higher)
#' compare_ranks(c(12, 11, 9), c(12, 11, 10))  # Returns -1 (second is higher)
#' compare_ranks(c(12, 11, 10), c(12, 11, 10)) # Returns 0 (equal)
compare_ranks <- function(ranks1, ranks2) {
  for (i in 1:min(length(ranks1), length(ranks2))) {
    if (ranks1[i] != ranks2[i]) {
      return(sign(ranks1[i] - ranks2[i]))
    }
  }
  return(0)
}

#' Get hand type name from numeric score
#'
#' Converts a numeric hand score (0-8) into a readable hand type name.
#'
#' @param score Numeric hand score (0-8 where 0=High Card, 8=Straight Flush)
#' @return Character string with hand type name
#' @export
#' @examples
#' get_hand_name(0)  # "High Card"
#' get_hand_name(1)  # "One Pair"
#' get_hand_name(8)  # "Straight Flush"
#' get_hand_name(99) # "Unknown"
get_hand_name <- function(score) {
  names <- c("High Card", "One Pair", "Two Pair", "Three of a Kind",
             "Straight", "Flush", "Full House", "Four of a Kind", "Straight Flush")
  if (score >= 0 && score <= 8) {
    return(names[score + 1])
  }
  return("Unknown")
}

#' Get base win strength from hand score
#'
#' Returns the estimated win probability for a given hand type against a single opponent.
#' These are pre-calculated probabilities based on poker hand strength analysis.
#'
#' @param score Numeric hand score (0-8)
#' @return Numeric base win probability (0-1)
#' @export
#' @examples
#' get_base_strength(0)  # 0.15 (High Card)
#' get_base_strength(1)  # 0.42 (One Pair)
#' get_base_strength(8)  # 0.98 (Straight Flush)
get_base_strength <- function(score) {
  # Pre-calculated win rates for different hand types
  strengths <- c(0.15, 0.42, 0.58, 0.72, 0.78, 0.82, 0.88, 0.95, 0.98)
  if (score >= 0 && score <= 8) {
    return(strengths[score + 1])
  }
  return(0.15)
}
