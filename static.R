# library(colorscience)
library(distr)
library(tidyverse)

# load required data
colorscience_data <- data(package = "colorscience")[[3]][, 3]
data(list = colorscience_data, package = "colorscience")

# define absorbance peaks
wavelength1 <- 450
width1 <- 20

wavelength2 <- 660
width2 <- 20

# select incident light
light <- "E"

# transform incident light spectrum to count data
inc_light <- as_tibble(illuminants[, c("wlnm", light)]) %>%
  rename("intensity" = .data[[light]]) %>%
  uncount(intensity)

# create pseudo absorbance spectrum as distribution
spec_dist <- UnivarMixingDistribution(
  Norm(mean = wavelength1, sd = width1),
  Norm(mean = wavelength2, sd = width2),
  mixCoeff = c(0.3, 0.2)
)

# create sampler
spec_dist_sampler <- r(spec_dist)

# sample from distribution
spec_dist_sample <- as_tibble(spec_dist_sampler(1e6))

# plot incident light spectrum (distribution)
ggplot(inc_light) +
  geom_density(aes(x = wlnm)) 

# plot compound absorbance spectrum (distribution)
ggplot(spec_dist_sample) +
  geom_density(aes(x = value)) 

# transfrom absorbance from a distribution into a spectrum and then into transmittance
spec <- spec_dist_sample %>%
  filter(value >= 377.5 & value <= 782.5) %>%
  mutate(
    bin = cut_width(value, width = 5, boundary = 377.5, labels = F), # cut into 5 nm bins
    bin = 380 + ((bin - 1) * 5) # name the bins by the central wavelength
  ) %>%
  group_by(bin) %>%
  tally() %>%
  mutate(
    n = (n / max(n)) * 2, # range normalise to pseudo absorbance
    n = (10^-n) * 100 # transform into transmittance
  )

# transform transmittance into a matrix
spec_mat <- data.matrix(spec)

# transform transmittance spectrum into XYZ colour coordinates and then RGB values
XYZ_col <- colorscience::spectra2XYZ(spec_mat, illuminantIn = illuminants[, c("wlnm", light)])
RGB_col <- colorscience::XYZ2RGB(XYZ_col, illuminant = light)
# RGB_col <- colorscience::XYZtoRGB(XYZ_col[1], XYZ_col[2], XYZ_col[3])

# correct RGB values < 0 and > 1 (unclear to me why they exist)
RGB_col <- ifelse(RGB_col < 0, 0, ifelse(RGB_col > 1, 1, RGB_col))

# plot the colour of the compound
ggplot() + 
  theme_void() +
  annotate("point",
           shape = 21,
           size = 50,
           fill = rgb(RGB_col),
           x = 1,
           y = 1) +
  annotate("text",
           label = rgb(RGB_col),
           x = 1,
           y = 1)
