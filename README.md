
# pokerhand2analyzR

<!-- badges: start -->
<!-- badges: end -->

The goal of pokerhand2analyzR is to ...

## Installation

You can install the development version of pokerhand2analyzR like so:

``` r
remotes::install_github("finqen/pokerhand2analyzR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(pokerhand2analyzR)
ls("package:pokerhand2analyzR")

# Main functions - rest are just helper functions for these two

# find_threat_hands
?find_threat_hands

find_threat_hands("AS KH", "QS JS 7S")
find_threat_hands("QH QS", "9C TD JH", 2)

# evaluate_hand_strength
?evaluate_hand_strength

evaluate_hand_strength("AS AH", "9H TC JC")
evaluate_hand_strength("QH QS", "9C TD JH AS KC", 2)

```

## Map for cards

Cards are represented as simple strings that get converted to internal numeric values for processing.

### Input Format
Cards are written as **two characters**: `[Rank][Suit]`

```r
"AS"    # Ace of Spades
"KH"    # King of Hearts  
"QC"    # Queen of Clubs
"JD"    # Jack of Diamonds
"TC"    # Ten of Clubs
"9S"    # Nine of Spades
"2H"    # Two of Hearts
```

### Rank Mapping
| Card | Internal Value | Description |
|------|---------------|-------------|
| `2`  | 0             | Two         |
| `3`  | 1             | Three       |
| `4`  | 2             | Four        |
| `5`  | 3             | Five        |
| `6`  | 4             | Six         |
| `7`  | 5             | Seven       |
| `8`  | 6             | Eight       |
| `9`  | 7             | Nine        |
| `T`  | 8             | Ten         |
| `J`  | 9             | Jack        |
| `Q`  | 10            | Queen       |
| `K`  | 11            | King        |
| `A`  | 12            | Ace         |

### Suit Mapping
| Symbol | Suit     |
|--------|----------|
| `H`    | Hearts   |
| `D`    | Diamonds |
| `C`    | Clubs    |
| `S`    | Spades   |

### Special Notes

- **Ten notation**: Use `T` for ten (not `10`)
