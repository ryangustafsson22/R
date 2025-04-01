#11FEB2025 Chapter 1 and 2

#Prerequisites (Packages and Libraries)
# install.packages("tidyverse")
# library(tidyverse)
# tidyverse_update()
# install.packages(
#   c("arrow", "babynames", "curl", "duckdb", "gapminder", 
#     "ggrepel", "ggridges", "ggthemes", "hexbin", "janitor", "Lahman", 
#     "leaflet", "maps", "nycflights13", "openxlsx", "palmerpenguins", 
#     "repurrrsive", "tidymodels", "writexl")
# )
# library(palmerpenguins)
# library(ggthemes)

#First Steps
penguins
View(penguins)

ggplot(data = penguins)
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
)
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point()
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm")
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm")
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()


# Section 1.3-----------
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

penguins |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

ggplot(penguins, aes(x = species)) +
  geom_bar()

ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()

ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75)

ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5)

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)

# Start of Chapter 2 Executables
1 / 200 * 30
(59 + 73 + 2) / 3
sin(pi / 2)
x <- 3 * 4
primes <- c(2, 3, 5, 7, 11, 13)
primes * 2
#> [1]  4  6 10 14 22 26
primes - 1
#> [1]  1  2  4  6 10 12
object_name <- value

# create vector of primes
primes <- c(2, 3, 5, 7, 11, 13)

# multiply primes by 2
primes * 2
#> [1]  4  6 10 14 22 26

x
#> [1] 12

this_is_a_really_long_name <- 2.5
this_is_a_really_long_name

r_rocks <- 2^3

r_rock
#> Error: object 'r_rock' not found
R_rocks
#> Error: object 'R_rocks' not found

function_name(argument1 = value1, argument2 = value2, ...)

seq(from = 1, to = 10)
seq(1, 10)

x <- "hello world"

# Chapter Exercises

# Chapter 1.2.5
#1
View(penguins)

#3
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) + 
  geom_point()

#4
penguins |> 
  ggplot(aes(x = species, y = bill_depth_mm)) + 
  geom_point()

#5
ggplot(data = penguins) + 
  geom_point()

#6
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) + 
  geom_point(na.rm = TRUE)

#7
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) + 
  geom_point(na.rm = TRUE) +
  labs(caption = "Data come from the palmerpenguins package")

#8
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = bill_depth_mm)) +
  geom_smooth(method = "loess")

#9
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)

#10
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )

# Chapter 1.4.3 Exercises
#1
ggplot(penguins, aes(y = species)) +
  geom_bar()

#2
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")

ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")

#4
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = .1)

# Chapter 1.5.5 Exercises
#1
?mpg

#2
ggplot(
  data = mpg,
  mapping = aes(x = hwy, y = displ, size = cty, color = cty)
) +
  geom_point()

#3
ggplot(
  data = mpg,
  mapping = aes(x = hwy, y = displ, linewidth = cty)
) +
  geom_point()

#5
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) + 
  geom_point()

#6
ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm, 
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species", shape = "Species")

#7
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")

# Chapter 1.6.1 Exercises
#1
ggplot(mpg, aes(x = class)) +
  geom_bar()
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave("mpg-plot.png")

# Chapter 2.5 Exercises
#1
my_variable <- 10
my_varıable
#> Error: object 'my_varıable' not found

#2
library(tidyverse)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(method = "lm")

#4
my_bar_plot <- ggplot(mpg, aes(x = class)) +
  geom_bar()
my_scatter_plot <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave(filename = "mpg-plot.png", plot = my_bar_plot)