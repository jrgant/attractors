## Example of the Lorenz attractor.
## Described in _Chaos_ by James Gleick (pub. 1987, 2008 ed.)
##
## Original paper:
## Lorenz E.N. Deterministic Nonperiodic Flow. Journal of the
## Atmospheric Sciences. 1963:20:130-41.

pacman::p_load(
  data.table,
  deSolve,
  ggplot2,
  ggthemes,
  gganimate,
  gridExtra,
  remotes
)

remotes::install_github("AckerDWM/gg3D")

## System of equations
loreq <- function(time, state, param) {
  with(as.list(c(state, param)), {
    dx_dt <- sigma * (Y - X)
    dy_dt <- X * (rho - Z) - Y
    dz_dt <- X * Y - beta * Z
    return(list(c(dx_dt, dy_dt, dz_dt)))
  })
}

param <- c(
  sigma = 12,
  rho = 28,
  beta = 8 / 3
)

## Lorenz's initialization
yini <- c(X = 10, Y = 10, Z = 10)
times <- seq(0, 100, by = 0.001)

out <- ode(yini, times, loreq, param)

outdt <- as.data.table(out)
names(outdt)

## plotly::plot_ly(outdt,
##                 x = ~X,
##                 y = ~Y,
##                 z = ~Z,
##                 color = ~time,
##                 mode = "lines")


plot_project <- function(xvar, yvar, data = outdt) {
  keepcol <- c("time", xvar, yvar)
  tmp <- data[, ..keepcol][, projection := paste0(xvar, yvar)][]
  setnames(tmp, c(xvar, yvar), c("plotx", "ploty"))
  tmp
}

allproj <- rbind(plot_project("X", "Z"),
                 plot_project("X", "Y"),
                 plot_project("Y", "Z"))

makeplot <- function(projpat = c("XZ", "XY", "YZ"), trip = F, data = allproj) {

  tmp <- data[projection %in% projpat]
  basep <- ggplot(tmp, aes(plotx, ploty, color = time)) +
    geom_path(linewidth = 0.2) +
    scale_color_viridis_c() +
    guides(color = "none") +
    theme_void()

  if (trip == TRUE) {
    basep +
      facet_wrap(vars(projection), nrow = 1, scales = "free") +
      theme(strip.text = element_blank())
  } else {
    basep
  }
}


l2d_xy <- makeplot("XY")
l2d_xz <- makeplot("XZ")
l2d_yz <- makeplot("YZ")
l2d_yt <- makeplot(trip = T)

basew <- 1.25
baseh <- 1.25
imgdir <- "~/.doom.d/splash"

ggsave(file.path(imgdir, "lorenz_2d_xz.png"),
       l2d_xz,
       width = basew,
       height = baseh)

ggsave(file.path(imgdir, "lorenz_2d_xy.png"),
       l2d_xy,
       width = basew,
       height = baseh)

ggsave(file.path(imgdir, "lorenz_2d_yz.png"),
       l2d_yz,
       width = basew,
       height = baseh)

ggsave(file.path(imgdir, "lorenz_2d_triptych.png"),
       l2d_yt,
       width = basew * 3,
       height = baseh)
