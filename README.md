
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
