#' Find potential threat hands based on board texture
#'
#' @param my_hand String like "AS KH" (my hole cards)
#' @param board String like "9H TC JC" (community cards)
#' @param opponents Number of opponents (default: 1)
#'
#' @return Data frame with threat types and probabilities
#' @export
#' @examples
#' # Basic flush threat detection
#' find_threat_hands("AS KH", "QS JS 7S")
#'
#' # Multiple threats with more opponents
#' find_threat_hands("QH QS", "9C TD JH", 2)
#'
#' # Paired board creating set threats
#' find_threat_hands("AS KH", "QQ 7C")
#'
#' # Connected board creating straight threats
#' find_threat_hands("AS KH", "9H TC JC")
find_threat_hands <- function(my_hand, board = "", opponents = 1) {
  if (board == "" || is.na(board)) {
    return(data.frame(
      threat = character(0),
      probability = numeric(0),
      description = character(0)
    ))
  }

  board_cards <- parse_cards(board)
  if (length(board_cards) == 0) {
    return(data.frame(
      threat = character(0),
      probability = numeric(0),
      description = character(0)
    ))
  }

  threats <- data.frame(
    threat = character(0),
    probability = numeric(0),
    description = character(0),
    stringsAsFactors = FALSE
  )

  # Analyze board for threats
  suits <- sapply(board_cards, function(x) x$suit)
  ranks <- sapply(board_cards, function(x) x$rank)

  # Flush threats
  suit_counts <- table(suits)
  max_suited <- max(suit_counts)
  if (max_suited >= 3) {
    flush_prob <- calculate_flush_threat(max_suited, opponents)
    threats <- rbind(threats, data.frame(
      threat = "Flush",
      probability = flush_prob,
      description = paste(max_suited, "suited cards on board"),
      stringsAsFactors = FALSE
    ))
  }

  # Straight threats
  if (length(unique(ranks)) >= 3) {
    straight_prob <- calculate_straight_threat(ranks, opponents)
    if (straight_prob > 0.05) {
      threats <- rbind(threats, data.frame(
        threat = "Straight",
        probability = straight_prob,
        description = "Connected board texture",
        stringsAsFactors = FALSE
      ))
    }
  }

  # Pair/set threats
  rank_counts <- table(ranks)
  max_paired <- max(rank_counts)
  if (max_paired >= 2) {
    set_prob <- calculate_set_threat(max_paired, opponents)
    threats <- rbind(threats, data.frame(
      threat = "Set/Full House",
      probability = set_prob,
      description = paste("Paired board (", max_paired, "of same rank)"),
      stringsAsFactors = FALSE
    ))
  }

  # High pair threats
  if (length(ranks) > 0) {
    overpair_prob <- calculate_overpair_threat(max(ranks), opponents)
    if (overpair_prob > 0.08) {
      threats <- rbind(threats, data.frame(
        threat = "Overpair",
        probability = overpair_prob,
        description = "Higher pocket pair",
        stringsAsFactors = FALSE
      ))
    }
  }

  # Sort by probability and return
  if (nrow(threats) > 0) {
    threats <- threats[order(-threats$probability), ]
    threats$rank <- 1:nrow(threats)
    threats <- threats[, c("rank", "threat", "probability", "description")]
  }

  return(threats)
}

#' Calculate flush threat probability
#'
#' @param suited_count Number of suited cards on board
#' @param opponents Number of opponents
#' @return Numeric probability of flush threat
#' @export
calculate_flush_threat <- function(suited_count, opponents) {
  base_probs <- c(0, 0, 0, 0.12, 0.35, 0.90)  # Index by suited_count
  if (suited_count <= 5) {
    base_prob <- base_probs[suited_count + 1]
  } else {
    base_prob <- 0.95
  }

  # Adjust for opponents
  return(min(0.95, base_prob * (1 + opponents * 0.15)))
}

#' Calculate straight threat probability
#'
#' @param ranks Vector of card ranks on board
#' @param opponents Number of opponents
#' @return Numeric probability of straight threat
#' @export
calculate_straight_threat <- function(ranks, opponents) {
  # Check for consecutive sequences
  sorted_ranks <- sort(unique(ranks))
  max_consecutive <- 1
  current_consecutive <- 1

  if (length(sorted_ranks) > 1) {
    for (i in 2:length(sorted_ranks)) {
      if (sorted_ranks[i] - sorted_ranks[i-1] == 1) {
        current_consecutive <- current_consecutive + 1
      } else {
        max_consecutive <- max(max_consecutive, current_consecutive)
        current_consecutive <- 1
      }
    }
    max_consecutive <- max(max_consecutive, current_consecutive)
  }

  base_prob <- max(0, (max_consecutive - 2) * 0.08)  # 8% per connected card above 2
  return(min(0.70, base_prob * (1 + opponents * 0.20)))
}

#' Calculate set/full house threat probability
#'
#' @param pair_count Number of cards of same rank on board
#' @param opponents Number of opponents
#' @return Numeric probability of set/full house threat
#' @export
calculate_set_threat <- function(pair_count, opponents) {
  if (pair_count == 2) {
    base_prob <- 0.08  # Pair on board
  } else if (pair_count == 3) {
    base_prob <- 0.25  # Trips on board
  } else {
    base_prob <- 0.50  # Quads on board
  }

  return(min(0.85, base_prob * (1 + opponents * 0.10)))
}

#' Calculate overpair threat probability
#'
#' @param max_board_rank Highest rank on board (0-12, where 12 = Ace)
#' @param opponents Number of opponents
#' @return Numeric probability of overpair threat
#' @export
calculate_overpair_threat <- function(max_board_rank, opponents) {
  # Higher ranks available (12 = Ace)
  higher_ranks <- max(0, 12 - max_board_rank)
  base_prob <- higher_ranks * 0.015  # 1.5% per higher rank

  # Adjust for multiple opponents
  total_prob <- 1 - ((1 - base_prob) ^ opponents)
  return(min(0.40, total_prob))
}
