pacman::p_load(data.table, ggplot2, ggthemes, gganimate)
## Henon's attractor
## Describe in _Chaos_ by James Gleick (pub. 1988, 2008 ed.), figure on page 151

xnew <- function(x, y) y + 1 - 1.4 * x^2
ynew <- function(x) 0.3 * x

N <- 1500
x_start <- 0.00001
y_start <- 0.00005

x <- rep(NA, N)
y <- rep(NA, N)

for (i in 1:N) {
  if (i == 1) {
    x[i] <- x_start
    y[i] <- y_start
  } else {
    x[i] <- xnew(x[i-1], y[i-1])
    y[i] <- ynew(x[i-1])
  }
}

dt <- data.table(id = 1:N, x = x, y = y)

hpanim <- dt |>
  ggplot(aes(x = x, y = y, group = id)) +
  geom_point(size = 1) +
  theme_void() +
  transition_reveal(id) +
  enter_fade()

print(hpanim)

gganimate::anim_save("hpanim.gif", hpanim)

# fwrite(dt, here::here("henon_attractor.csv"))
