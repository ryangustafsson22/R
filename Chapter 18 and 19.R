
# Chapter 18 Executables --------------------------------------------------

#18.2
treatment <- tribble(
  ~person,           ~treatment, ~response,
  "Derrick Whitmore", 1,         7,
  NA,                 2,         10,
  NA,                 3,         NA,
  "Katherine Burke",  1,         4
)

treatment |>
  fill(everything())

x <- c(1, 4, 5, 7, NA)
coalesce(x, 0)

x <- c(1, 4, 5, 7, -99)
na_if(x, -99)

x <- c(NA, NaN)
x * 10
x == 1
is.na(x)

0 / 0 
#> [1] NaN
0 * Inf
#> [1] NaN
Inf - Inf
#> [1] NaN
sqrt(-1)
#> Warning in sqrt(-1): NaNs produced
#> [1] NaN

#18.3

stocks <- tibble(
  year  = c(2020, 2020, 2020, 2020, 2021, 2021, 2021),
  qtr   = c(   1,    2,    3,    4,    2,    3,    4),
  price = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

stocks |>
  pivot_wider(
    names_from = qtr, 
    values_from = price
  )

stocks |>
  complete(year, qtr)
stocks |>
  complete(year = 2019:2021, qtr)

flights |> 
  distinct(faa = dest) |> 
  anti_join(airports)

flights |> 
  distinct(tailnum) |> 
  anti_join(planes)

#18.4

health <- tibble(
  name   = c("Ikaia", "Oletta", "Leriah", "Dashay", "Tresaun"),
  smoker = factor(c("no", "no", "no", "no", "no"), levels = c("yes", "no")),
  age    = c(34, 88, 75, 47, 56),
)

health |> count(smoker)
health |> count(smoker, .drop = FALSE)

ggplot(health, aes(x = smoker)) +
  geom_bar() +
  scale_x_discrete()

ggplot(health, aes(x = smoker)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

health |> 
  group_by(smoker, .drop = FALSE) |> 
  summarize(
    n = n(),
    mean_age = mean(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age)
  )

x1 <- c(NA, NA)
length(x1)

x2 <- numeric()
length(x2)

health |> 
  group_by(smoker) |> 
  summarize(
    n = n(),
    mean_age = mean(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age)
  ) |> 
  complete(smoker)


# Chapter 19 Exercises ----------------------------------------------------

#19.2 Keys
airlines
airports
planes
weather

planes |> 
  count(tailnum) |> 
  filter(n > 1)
weather |> 
  count(time_hour, origin) |> 
  filter(n > 1)

planes |> 
  filter(is.na(tailnum))
weather |> 
  filter(is.na(time_hour) | is.na(origin))

flights |> 
  count(time_hour, carrier, flight) |> 
  filter(n > 1)

airports |>
  count(alt, lat) |> 
  filter(n > 1)

flights2 <- flights |> 
  mutate(id = row_number(), .before = 1)
flights2

#19.3
flights2 <- flights |> 
  select(year, time_hour, origin, dest, tailnum, carrier)
flights2

flights2 |>
  left_join(airlines)

flights2 |> 
  left_join(weather |> select(origin, time_hour, temp, wind_speed))

flights2 |> 
  left_join(planes |> select(tailnum, type, engines, seats))

flights2 |> 
  left_join(planes)
flights2 |> 
  left_join(planes, join_by(tailnum))

flights2 |> 
  left_join(airports, join_by(dest == faa))
flights2 |> 
  left_join(airports, join_by(origin == faa))

airports |> 
  semi_join(flights2, join_by(faa == origin))

airports |> 
  semi_join(flights2, join_by(faa == dest))

airports |> 
  semi_join(flights2, join_by(faa == dest))

flights2 |>
  anti_join(planes, join_by(tailnum)) |> 
  distinct(tailnum)

#19.4
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

df1 <- tibble(key = c(1, 2, 2), val_x = c("x1", "x2", "x3"))
df2 <- tibble(key = c(1, 2, 2), val_y = c("y1", "y2", "y3"))
df1 |> 
  inner_join(df2, join_by(key))

#19.5
x |> inner_join(y, join_by(key == key), keep = TRUE)

df <- tibble(name = c("John", "Simon", "Tracy", "Max"))
df |> cross_join(df)

df <- tibble(id = 1:4, name = c("John", "Simon", "Tracy", "Max"))
df |> inner_join(df, join_by(id < id))

parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03"))
)
set.seed(123)
employees <- tibble(
  name = sample(babynames::babynames$name, 100),
  birthday = ymd("2022-01-01") + (sample(365, 100, replace = TRUE) - 1)
)
employees

employees |> 
  left_join(parties, join_by(closest(birthday >= party)))

parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03")),
  start = ymd(c("2022-01-01", "2022-04-04", "2022-07-11", "2022-10-03")),
  end = ymd(c("2022-04-03", "2022-07-11", "2022-10-02", "2022-12-31"))
)
parties

parties |> 
  inner_join(parties, join_by(overlaps(start, end, start, end), q < q)) |> 
  select(start.x, end.x, start.y, end.y)

parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03")),
  start = ymd(c("2022-01-01", "2022-04-04", "2022-07-11", "2022-10-03")),
  end = ymd(c("2022-04-03", "2022-07-10", "2022-10-02", "2022-12-31"))
)

employees |> 
  inner_join(parties, join_by(between(birthday, start, end)), unmatched = "error")


# Chapter 18 Exercises ----------------------------------------------------

#1
missing_planes <- flights |>
  filter(!tailnum %in% planes$tailnum)

missing_per_carrier <- missing_planes |>
  group_by(carrier) |>
  summarize(num_missing = n()) |>
  arrange(desc(num_missing))
missing_per_carrier


# Chapter 19 Exercises ----------------------------------------------------

#19.2.4
#3
weather |>
  group_by(year, month, day, hour, origin) |>
  filter(n() > 1) |>
  arrange(year, month, day, hour, origin)

#4 
special_days <- tibble(
  month = c(1, 7, 11, 12, 12, 12),
  day   = c(1, 4, 28, 24, 25, 31),
  holiday = c("New Year's Day", "Independence Day", "Thanksgiving", 
              "Christmas Eve", "Christmas Day", "New Year's Eve")
)

#5
View(Batting)
View(People)
View(Salaries)

View(People)
View(Managers)
View(AwardsManagers)

View(Batting)
View(Pitching)
View(Fielding)

#19.3.4
#1
flights <- flights |>
  mutate(datetime = make_datetime(year, month, day, hour))
flights_48hours <- flights |>
  mutate(time_group = floor_date(datetime, "2 days")) |>
  group_by(time_group) |>
  summarize(total_delay = sum(dep_delay, na.rm = TRUE)) |>
  arrange(desc(total_delay))
flights_48hours
View(weather)  

#2
flights2 <- flights |> 
  select(year, time_hour, origin, dest, tailnum, carrier)
top_dest <- flights2 |>
  count(dest, sort = TRUE) |>
  head(10)
top_dest_codes <- top_dest$dest

flights2 |>
  filter(dest %in% top_dest_codes)

#3
weather <- weather |>
  mutate(datetime = make_datetime(year, month, day, hour))
flights |>
  anti_join(weather, join_by(origin, datetime))

#4
unmatched_tailnums <- flights |>
  anti_join(planes, join_by(tailnum))

unmatched_tailnums |>
  count(is.na(tailnum) | tailnum == "")

#5
flights_planes <- flights |>
  filter(!is.na(tailnum)) |>
  select(tailnum, carrier) |>
  distinct()

carrier_planes <- flights_planes |>
  group_by(tailnum) |>
  summarize(
    carriers = paste(sort(unique(carrier)), collapse = ", "),
    n_carriers = n_distinct(carrier)
  )

planes_with_carriers <- planes |>
  left_join(carrier_planes, join_by(tailnum))
planes_with_carriers |>
  filter(n_carriers > 1)

#6
flights_geolocations <- flights |>
  left_join(airports, join_by(origin == faa))

flights_geolocations <- flights_geolocations |>
  rename(
    origin_lat = lat,
    origin_lon = lon)

flights_geolocations <- flights_geolocations |>
  left_join(airports, join_by(dest == faa))

flights_geolocations <- flights_geolocations |>
  rename(
    dest_lat = lat,
    dest_lon = lon)

#7
avg_delay <- flights |>
  group_by(dest) |>
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE), .groups = "drop")

avg_delay_airports <- avg_delay |>
  inner_join(airports, join_by(dest == faa))

ggplot(avg_delay_airports, aes(x = lon, y = lat, color = avg_arr_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

#8
flights_june13 <- flights |>
  filter(month == 6 & day == 13)

avg_delay <- flights_june13 |>
  group_by(dest) |>
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE), .groups = "drop")

avg_delay_airports <- avg_delay |>
  inner_join(airports, join_by(dest == faa))

ggplot(avg_delay_airports, aes(x = lon, y = lat, color = avg_arr_delay)) +
  borders("state") +
  geom_point(size = 2) +
  coord_quickmap()

#19.5.5
#1
x |> full_join(y, join_by(key == key))

x |> full_join(y, join_by(key == key), keep = TRUE)
  
#2
parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03")),
  start = ymd(c("2022-01-01", "2022-04-04", "2022-07-11", "2022-10-03")),
  end = ymd(c("2022-04-03", "2022-07-11", "2022-10-02", "2022-12-31"))
)
parties |> 
  inner_join(parties, join_by(overlaps(start, end, start, end))) |> 
  select(start.x, end.x, start.y, end.y)
