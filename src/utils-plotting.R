# get_draw_alpha <- function(x)  boot::inv.logit(-0.5 + -0.005 * x)
get_draw_alpha <- function(x, p0 = 0.05, p1 = 0.5, p2 = 0, p3 = -250) {
  # p0: baseline (lowest) alpha
  # p1: alpha amplitude (max alpha is the sum of p0 and p1)
  # p2: the value of x at which the function is at its inflection point
  # p3: the length of x over which the function increases from 0.1 to 0.5 of its
  #     maximum
  p0 + p1 / (1 + exp((2.2 / p3) * (p2 - x)))
}
# x <- -500:500; plot(x, get_draw_alpha(x), type = 'l')
