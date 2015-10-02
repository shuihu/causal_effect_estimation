round.to.hundredth <- function(num) {
  rounded <- round(num / 0.01) * 0.01
  i = 3
  while (num != 0 && rounded == 0 && i <= 10) {
    rounded <- round(num, i)
    i = i + 1
  }
  rounded
}