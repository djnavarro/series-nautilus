
# set up ------------------------------------------------------------------

name <- "nautilus"
version <- 14
seeds <- 1400:1499

# define common helper functions & core tools
source(here::here("source", "common.R"), echo = FALSE)
source(here::here("source", "core.R"), echo = FALSE)
source(here::here("source", "bezier.R"), echo = FALSE)

make_art <- function(seed, name, version) {
  
  # specify the output path and message the user
  output <- output_path(name, version, seed, format = "png")
  message("generating art at ", output)
  
  set.seed(seed)
  
  # set up palettes
  palettes <- c("palette_01.csv", "palette_02.csv", "palette_03.csv") |> 
    purrr::map(
      \(x) here::here("source", x) |> 
        readr::read_csv(show_col_types = FALSE)
    ) |> 
    dplyr::bind_rows()
  
  # select a palette
  row <- sample.int(nrow(palettes), 1)
  palette <- unlist(palettes[row, ])
  palette <- sample(palette)
  
  # parameters that affect all bezier objects
  pull_1 <- runif(1, min = -.1, max = .1) * .25
  pull_2 <- runif(1, min = 0, max = .2) * .25
  x_mid <- runif(1, min = -2, max = 2)
  y_mid <- runif(1, min = -2, max = 2)
  width_scale <- runif(1, min = .5, max = 1) * 2
  length_scale <- runif(1, min = .5, max = 1) * .1
  cauchy_scale_a <- .05
  cauchy_scale_xy <- .01
  curl_scale <- .2
  
  get_curl <- function(x, y, seed = NULL) {
    ambient::curl_noise(
      generator = ambient::fracture,
      noise = ambient::gen_simplex,
      fractal = ambient::fbm,
      x = x,
      y = y,
      frequency = .8,
      seed = seed,
      octaves = 5
    )
  }

  n_ribbons <- 4000
  
  base <- tibble::tibble(
    r = rbeta(n_ribbons, 10, 1) * 1.5,
    a = rcauchy(n_ribbons, scale = cauchy_scale_a) * pi + runif(1, min = 0, max = 2*pi),
    x_jtr = rcauchy(n_ribbons, 0, cauchy_scale_xy) * r,
    y_jtr = rcauchy(n_ribbons, 0, cauchy_scale_xy) * r,
    x = r * cos(a) + x_jtr,
    y = r * sin(a) + y_jtr,
    sh = runif(n_ribbons, .75, 1.25) * length_scale,
    x2 = r * cos(a + sh) + x_jtr,
    y2 = r * sin(a + sh) + y_jtr,
  )
  curl <- get_curl(base$x, base$y, seed) |> 
    dplyr::mutate(
      x = ambient::normalize(x, to = c(-1, 1)),
      y = ambient::normalize(y, to = c(-1, 1))
    )
  
  values <- tibble::tibble(
    x = base$x2,
    y = base$y2,
    xend = base$x + curl$x * curl_scale,
    yend = base$y + curl$y * curl_scale,
    xctr_1 = (1 - pull_1) * (x + xend)/2 + pull_1 * x_mid,
    yctr_1 = (1 - pull_1) * (y + yend)/2 + pull_1 * y_mid,
    xctr_2 = (x + xend) / 2 + pull_2 * runif(n_ribbons, min = -2, max = 2),
    yctr_2 = (y + yend) / 2 + pull_2 * runif(n_ribbons, min = -2, max = 2),
    width = width_scale * runif(n_ribbons, min = 0, max = 1),
    smooth = 6L,
    n = 100L,
    fill = sample(palette, n_ribbons, replace = TRUE),
    color = fill
  )

  # list of things to draw
  drawables <- purrr::pmap(values, bezier_ribbon)
  
  # draw sketch and save it
  r <- 2
  seed_str <- stringr::str_pad(seed, width = 4, pad = "0")
  png(
    filename = output,
    width = 2000,
    height = 2000,
    units = "px",
    bg = palette[1]
  )
  drawables |>
    sketch() |>
    draw(xlim = c(-r, r), ylim = c(-r, r))
  dev.off()
  
}

for(s in seeds) make_art(s, name, version)

