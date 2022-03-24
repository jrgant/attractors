## Example of the logistic equation studied by Yorke and May.
## Described in _Chaos_ by James Gleick (pub. 1987, 2008 ed.), figure on page 71

pacman::p_load(data.table, ggplot2, ggthemes, gganimate, doParallel)

logfun <- function(x, r, N) {
  xt <- rep(NA, N)
  for (i in 1:N) {
    if (i == 1) {
      xt[i] <- x
    } else {
      xt[i] <- r * xt[i - 1] * (1 - xt[i - 1])
    }
  }
  return(xt)
}

# Vary both x (population size) and r
xr_grid <- expand.grid(x = seq(0, 1, 0.01), r = seq(0, 4, 0.001))

tail(xr_grid)

ncore <- parallel::detectCores() - 1
doParallel::registerDoParallel(cores = ncore)

evals_xr <- foreach(i = seq_len(nrow(xr_grid)), .combine = "c") %dopar% {
  tev <- logfun(x = xr_grid$x[i], r = xr_grid$r[i], N = 1000)[1000]
  tev
}

doParallel::stopImplicitCluster()

dt <- as.data.table(xr_grid)
dt[, x_eval := evals_xr][]

logplot <- dt |>
  ggplot(aes(x = r, y = x_eval)) +
  geom_point(size = 0.1) +
  labs(y = "x at time 1,000") +
  theme_tufte(base_size = 30, ticks = F)

ggsave(
  here::here("images", "logistic.pdf"),
  logplot,
  width = 16,
  height = 12
)
