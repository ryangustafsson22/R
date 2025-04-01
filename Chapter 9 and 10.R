
# Chapter 9 ---------------------------------------------------------------

#ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(
#    mapping = aes(<MAPPINGS>),
#    stat = <STAT>, 
#    position = <POSITION>
#  ) +
#  <COORDINATE_FUNCTION> +
#  <FACET_

mpg

ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, shape = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, size = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, alpha = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "blue")

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "blue", size = 4, shape = 8)

#Chapter 9.3 Geometric Objects

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy, shape = drv)) + 
  geom_smooth()
#does a line for each drive (4WD, RWD, etc.)^^^^

ggplot(mpg, aes(x = displ, y = hwy, linetype = drv)) + 
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) + 
  geom_point() +
  geom_smooth(aes(linetype = drv))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(group = drv))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(color = drv))

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) + 
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_point(
    data = mpg |> filter(class == "2seater"), 
    color = "red"
  ) +
  geom_point(
    data = mpg |> filter(class == "2seater"), 
    shape = "circle open", size = 3, color = "red"
  )

ggplot(mpg, aes(x = hwy)) +
  geom_histogram(binwidth = 2)

ggplot(mpg, aes(x = hwy)) +
  geom_density()

ggplot(mpg, aes(x = hwy)) +
  geom_boxplot()

library(ggridges)

ggplot(mpg, aes(x = hwy, y = drv, fill = drv, color = drv)) +
  geom_density_ridges(alpha = 0.5, show.legend = FALSE)

#Chapter 9.4 Facets
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_wrap(~cyl)

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(drv ~ cyl)

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(drv ~ cyl, scales = "free")

ggplot(diamonds, aes(x = cut)) + 
  geom_bar()

diamonds |>
  count(cut) |>
  ggplot(aes(x = cut, y = n)) +
  geom_bar(stat = "identity")

ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 1)) + 
  geom_bar()

ggplot(diamonds) + 
  stat_summary(
    aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

#Chapter 9.6 Positions
ggplot(mpg, aes(x = drv, color = drv)) + 
  geom_bar()

ggplot(mpg, aes(x = drv, fill = drv)) + 
  geom_bar()

ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar()

ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(alpha = 1/5, position = "identity")

ggplot(mpg, aes(x = drv, color = class)) + 
  geom_bar(fill = NA, position = "identity")

ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(position = "fill")

ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(position = "dodge")

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(position = "jitter")

#9.7 Coordinate systems

nz <- map_data("nz")

ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black")

ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = clarity, fill = clarity), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1)

bar + coord_flip()
bar + coord_polar()


# Chapter 10 --------------------------------------------------------------

ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.5)

smaller <- diamonds |> 
  filter(carat < 3)

ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(diamonds, aes(x = y)) + 
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds |> 
  filter(y < 3 | y > 20) |> 
  select(price, x, y, z) |>
  arrange(y)
unusual

diamonds2 <- diamonds |> 
  filter(between(y, 3, 20))

diamonds2 <- diamonds |> 
  mutate(y = if_else(y < 3 | y > 20, NA, y))

ggplot(diamonds2, aes(x = x, y = y)) + 
  geom_point()

ggplot(diamonds2, aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)

nycflights13::flights |> 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |> 
  ggplot(aes(x = sched_dep_time)) + 
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4)

ggplot(diamonds, aes(x = price)) + 
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 0.75)

ggplot(diamonds, aes(x = price, y = after_stat(density))) + 
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 0.75)

ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(mpg, aes(x = fct_reorder(class, hwy, median), y = hwy)) +
  geom_boxplot()

ggplot(mpg, aes(x = hwy, y = fct_reorder(class, hwy, median))) +
  geom_boxplot()

ggplot(diamonds, aes(x = cut, y = color)) +
  geom_count()

diamonds |> 
  count(color, cut) |>  
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = n))

ggplot(smaller, aes(x = carat, y = price)) +
  geom_point()

ggplot(smaller, aes(x = carat, y = price)) + 
  geom_point(alpha = 1 / 100)

ggplot(smaller, aes(x = carat, y = price)) +
  geom_bin2d()
# install.packages("hexbin")
ggplot(smaller, aes(x = carat, y = price)) +
  geom_hex()

ggplot(smaller, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_width(carat, 0.1)))

diamonds |> 
  filter(x >= 4) |> 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

ggplot(smaller, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_number(carat, 20)))

library(tidymodels)

diamonds <- diamonds |>
  mutate(
    log_price = log(price),
    log_carat = log(carat)
  )

diamonds_fit <- linear_reg() |>
  fit(log_price ~ log_carat, data = diamonds)

diamonds_aug <- augment(diamonds_fit, new_data = diamonds) |>
  mutate(.resid = exp(.resid))

ggplot(diamonds_aug, aes(x = carat, y = .resid)) + 
  geom_point()

ggplot(diamonds_aug, aes(x = cut, y = .resid)) + 
  geom_boxplot()



# Chapter 9 Exercises -----------------------------------------------------



# Chapter 9.2.1 Exercises -------------------------------------------------

#1
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "pink", shape = 17)
#2
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, color = blue))

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "blue")

#4
ggplot(mpg, aes(x = displ, y = hwy, color = displ < 5)) + 
  geom_point()


# Chapter 9.3.1 Exercises -------------------------------------------------

#2
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(color = drv), show.legend = FALSE)

#3
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(color = drv), show.legend = FALSE, se = TRUE)

#4
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy, line = drv)) + 
  geom_point() + 
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy, line = drv, color = drv)) + 
  geom_point() + 
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = drv)) + 
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) + 
  geom_point() +
  geom_smooth(aes(linetype = drv))

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) + 
  geom_point(stroke = 2)


# Chapter 9.4.1 Exercises -------------------------------------------------
#1
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_wrap(~displ)

#2
ggplot(mpg) + 
  geom_point(aes(x = drv, y = cyl))

#3
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

#4
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) + 
  facet_wrap(~ cyl, nrow = 2)

#6
ggplot(mpg, aes(x = displ)) + 
  geom_histogram() + 
  facet_grid(drv ~ .)

ggplot(mpg, aes(x = displ)) + 
  geom_histogram() +
  facet_grid(. ~ drv)

#7
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(drv ~ .)


# Chapter 9.5.1 Exercises -------------------------------------------------

#1
ggplot(diamonds, aes(x = cut, y = depth) +
  geom_point()
  
#5
ggplot(diamonds, aes(x = cut, y = after_stat(prop))) + 
  geom_bar()
ggplot(diamonds, aes(x = cut, fill = color, y = after_stat(prop))) + 
  geom_bar()


# Chapter 9.6.1 Exercises -------------------------------------------------

#1
ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point()

#2
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(position = "identity")

#5
ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot()


# Chapter 9.7.1 Exercises -------------------------------------------------

#1
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = clarity, fill = clarity), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1)

bar + coord_flip()
bar + coord_polar()

#3
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed()


# Chapter 10 Exercises ----------------------------------------------------


# Chapter 10.3.3 Exercises ------------------------------------------------

#1
ggplot(diamonds, aes(x = x)) + 
  geom_histogram(binwidth = 0.5)

ggplot(diamonds, aes(x = y)) + 
  geom_histogram(binwidth = 0.5)

ggplot(diamonds, aes(x = z)) + 
  geom_histogram(binwidth = 0.5)

#2
ggplot(diamonds, aes(x = price)) + 
  geom_histogram(binwidth = 5)

#3
diamonds |>
  filter(carat == .99) |>
  count()
diamonds |>
  filter(carat == 1) |>
  count()

#4
ggplot(diamonds, aes(x = x)) + 
  geom_histogram(binwidth = 0.5) +
  coord_cartesian()

ggplot(diamonds, aes(x = x)) + 
  geom_histogram(binwidth = 0.5) +
  xlim(0, 20)

ggplot(diamonds, aes(x = x)) + 
  geom_histogram() +
  ylim(0, 10000)


# Chapter 10.4.1 Exercises ------------------------------------------------

#2
diamonds |>
  mutate(avg_x = mean(x, na.rm = TRUE))

#3
flights2 <- flights |> 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  )
ggplot(flights2, aes(x = sched_dep_time)) + 
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4) +
  facet_grid(cancelled, scales = "free")

#Don't know the issue here


# Chapter 10.5.1.1 Exercises ----------------------------------------------

#1
nycflights13::flights |> 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |> 
  ggplot(aes(x = sched_dep_time, y = after_stat(density))) + 
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4)

#3
ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip()

#4
ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip() +
  geom_lv()

#5
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_violin(aes(fill = cut))

ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 600, aes(fill = cut), position = "dodge") +
  facet_wrap(~ cut, scales = "free_y")

ggplot(diamonds, aes(x = price, color = cut)) +
  geom_freqpoly(binwidth = 200)

ggplot(diamonds, aes(x = price, fill = cut)) +
  geom_density(alpha = 0.25)


# Chapter 10.5.2.1 Exercises ----------------------------------------------

#1
diamonds |> 
  count(color, cut) |>
  group_by(cut) |>
  mutate(proportion = n/sum(n) |>
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = proportion))
 #couldn't get this to work either
 
#2
ggplot(diamonds, aes(x = color, fill = cut)) +
  geom_bar()

diamonds |>
  count(color, cut)

#3
avg_delays <- flights |> 
  group_by(dest, month) |> 
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) 

ggplot(avg_delays, aes(x = month, y = dest, fill = avg_delay)) +
  geom_tile()


# Chapter 10.5.3.1 Exercises ----------------------------------------------

#2
ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(price, 0.1)))
#I was unable to find where the book got the smaller table from, but i believe
#this is the code that would partition the graph by price. 

#3
ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_width(carat, 0.1)))

#4
ggplot(diamonds, aes(x = carat, y = price, color = cut)) +
  facet_grid(cut ~ .) + 
  coord_cartesian()
#couldn't figure out why data points weren't showing up. 

#5
diamonds |> 
  filter(x >= 4) |> 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))
#6
ggplot(smaller, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_number(carat, 20)))
