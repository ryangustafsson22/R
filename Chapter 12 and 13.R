
# Chapter 12 Executables --------------------------------------------------

x <- c(1, 2, 3, 5, 7, 11, 13)
x * 2

df <- tibble(x)
df |> 
  mutate(y = x * 2)

flights |> 
  filter(dep_time > 600 & dep_time < 2000 & abs(arr_delay) < 20)

flights |> 
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
    .keep = "used"
  )

flights |> 
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
  ) |> 
  filter(daytime & approx_ontime)

x <- c(1 / 49 * 49, sqrt(2) ^ 2)
x

x == c(1, 2)

near(x, c(1, 2))

flights |> 
  filter(dep_time == NA)

is.na(c(TRUE, NA, FALSE))
#> [1] FALSE  TRUE FALSE
is.na(c(1, NA, 3))
#> [1] FALSE  TRUE FALSE
is.na(c("a", NA, "b"))
#> [1] FALSE  TRUE FALSE

flights |> 
  filter(is.na(dep_time))

flights |> 
  filter(month == 1, day == 1) |> 
  arrange(dep_time)

flights |> 
  filter(month == 1, day == 1) |> 
  arrange(desc(is.na(dep_time)), dep_time)

df <- tibble(x = c(TRUE, FALSE, NA))

df |> 
  mutate(
    and = x & NA,
    or = x | NA
  )

flights |> 
  filter(month == 11 | month == 12)

flights |> 
  filter(month == 11 | 12)

flights |> 
  mutate(
    nov = month == 11,
    final = nov | 12,
    .keep = "used"
  )

1:12 %in% c(1, 5, 11)

flights |> 
  filter(month %in% c(11, 12))

flights |> 
  filter(dep_time %in% c(NA, 0800))

flights |> 
  group_by(year, month, day) |> 
  summarize(
    all_delayed = all(dep_delay <= 60, na.rm = TRUE),
    any_long_delay = any(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  )

flights |> 
  group_by(year, month, day) |> 
  summarize(
    proportion_delayed = mean(dep_delay <= 60, na.rm = TRUE),
    count_long_delay = sum(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  )

flights |> 
  filter(arr_delay > 0) |> 
  group_by(year, month, day) |> 
  summarize(
    behind = mean(arr_delay),
    n = n(),
    .groups = "drop"
  )

flights |> 
  group_by(year, month, day) |> 
  summarize(
    behind = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    ahead = mean(arr_delay[arr_delay < 0], na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

x <- c(-3:3, NA)
if_else(x > 0, "+ve", "-ve")

if_else(x > 0, "+ve", "-ve", "???")

if_else(x < 0, -x, x)

x1 <- c(NA, 1, 2, NA)
y1 <- c(3, NA, 4, 6)
if_else(is.na(x1), y1, x1)

x <- c(-3:3, NA)
case_when(
  x == 0   ~ "0",
  x < 0    ~ "-ve", 
  x > 0    ~ "+ve",
  is.na(x) ~ "???"
)

case_when(
  x < 0 ~ "-ve",
  x > 0 ~ "+ve"
)

case_when(
  x < 0 ~ "-ve",
  x > 0 ~ "+ve",
  .default = "???"
)

case_when(
  x > 0 ~ "+ve",
  x > 2 ~ "big"
)

flights |> 
  mutate(
    status = case_when(
      is.na(arr_delay)      ~ "cancelled",
      arr_delay < -30       ~ "very early",
      arr_delay < -15       ~ "early",
      abs(arr_delay) <= 15  ~ "on time",
      arr_delay < 60        ~ "late",
      arr_delay < Inf       ~ "very late",
    ),
    .keep = "used"
  )



# Chapter 13 Executables -------------------------------------------------------------

x <- c("1.2", "5.6", "1e3")
parse_double(x)

x <- c("$1,234", "USD 3,513", "59%")
parse_number(x)

flights |> count(dest, sort = TRUE)

flights |> 
  group_by(dest) |> 
  summarize(
    n = n(),
    delay = mean(arr_delay, na.rm = TRUE)
  )

flights |> 
  group_by(dest) |> 
  summarize(carriers = n_distinct(carrier)) |> 
  arrange(desc(carriers))

flights |> 
  group_by(tailnum) |> 
  summarize(miles = sum(distance))

flights |> count(tailnum, wt = distance)

flights |> 
  group_by(dest) |> 
  summarize(n_cancelled = sum(is.na(dep_time))) 

x <- c(1, 2, 10, 20)
x / 5

x * c(1, 2)

x * c(1, 2, 3)

flights |> 
  filter(month == c(1, 2))

df <- tribble(
  ~x, ~y,
  1,  3,
  5,  2,
  7, NA,
)

df |> 
  mutate(
    min = pmin(x, y, na.rm = TRUE),
    max = pmax(x, y, na.rm = TRUE)
  )

df |> 
  mutate(
    min = min(x, y, na.rm = TRUE),
    max = max(x, y, na.rm = TRUE)
  )

1:10 %/% 3

1:10 %% 3

flights |> 
  mutate(
    hour = sched_dep_time %/% 100,
    minute = sched_dep_time %% 100,
    .keep = "used"
  )

flights |> 
  group_by(hour = sched_dep_time %/% 100) |> 
  summarize(prop_cancelled = mean(is.na(dep_time)), n = n()) |> 
  filter(hour > 1) |> 
  ggplot(aes(x = hour, y = prop_cancelled)) +
  geom_line(color = "grey50") + 
  geom_point(aes(size = n))

round(123.456, 1)

round(c(1.5, 2.5))

floor(x)
#> [1] 123
ceiling(x)
#> [1] 124

x <- 123.456
floor(x / 0.01) * 0.01
ceiling(x / 0.01) * 0.01
round(x / 4) * 4
round(x / 0.25) * 0.25

x <- c(1, 2, 5, 10, 15, 20)
cut(x, breaks = c(0, 5, 10, 15, 20))

cut(x, breaks = c(0, 5, 10, 100))

cut(x, 
    breaks = c(0, 5, 10, 15, 20), 
    labels = c("sm", "md", "lg", "xl")
)

x <- 1:10
cumsum(x)

x <- c(1, 2, 2, 3, 4, NA)
min_rank(x)

min_rank(desc(x))

df <- tibble(id = 1:10)

df |> 
  mutate(
    row0 = row_number() - 1,
    three_groups = row0 %% 3,
    three_in_each_group = row0 %/% 3
  )

x <- c(2, 5, 11, 11, 19, 35)
lag(x)
lead(x)

x - lag(x)
x == lag(x)

events <- tibble(
  time = c(0, 1, 2, 3, 5, 10, 12, 15, 17, 19, 20, 27, 28, 30)
)
events <- events |> 
  mutate(
    diff = time - lag(time, default = first(time)),
    has_gap = diff >= 5
  )
events

events |> mutate(
  group = cumsum(has_gap)
)

df <- tibble(
  x = c("a", "a", "a", "b", "c", "c", "d", "e", "a", "a", "b", "b"),
  y = c(1, 2, 3, 2, 4, 1, 3, 9, 4, 8, 10, 199)
)

df |> 
  group_by(id = consecutive_id(x)) |> 
  slice_head(n = 1)

flights |>
  group_by(year, month, day) |>
  summarize(
    mean = mean(dep_delay, na.rm = TRUE),
    median = median(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |> 
  ggplot(aes(x = mean, y = median)) + 
  geom_abline(slope = 1, intercept = 0, color = "white", linewidth = 2) +
  geom_point()

flights |>
  group_by(year, month, day) |>
  summarize(
    max = max(dep_delay, na.rm = TRUE),
    q95 = quantile(dep_delay, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

flights |> 
  group_by(origin, dest) |> 
  summarize(
    distance_iqr = IQR(distance), 
    n = n(),
    .groups = "drop"
  ) |> 
  filter(distance_iqr > 0)

flights |> 
  group_by(year, month, day) |> 
  summarize(
    first_dep = first(dep_time, na_rm = TRUE), 
    fifth_dep = nth(dep_time, 5, na_rm = TRUE),
    last_dep = last(dep_time, na_rm = TRUE)
  )

flights |> 
  group_by(year, month, day) |> 
  mutate(r = min_rank(sched_dep_time)) |> 
  filter(r %in% c(1, max(r)))

#x / sum(x) calculates the proportion of a total.
#(x - mean(x)) / sd(x) computes a Z-score (standardized to mean 0 and sd 1).
#(x - min(x)) / (max(x) - min(x)) standardizes to range [0, 1].
#x / first(x) computes an index based on the first observation.

# Chapter 12 Exercises ----------------------------------------------------

# 12.2.4 ------------------------------------------------------------------


#1
near
value <- sqrt(2) * 2
near(value, 2)

#2
NA_flights <- flights |>
  mutate(
    dep_time_na = is.na(dep_time),
    sched_dep_time_na = is.na(sched_dep_time),
    dep_delay_na = is.na(dep_delay))

NA_flights |>
  count(dep_time_na, sched_dep_time_na, dep_delay_na)


# 12.4.4 ------------------------------------------------------------------

#2
vector1 <- c(TRUE, FALSE, TRUE)
prod(vector1)

vector2 <- c(TRUE, TRUE, TRUE)
min(vector2)


# 12.5.4 ------------------------------------------------------------------

#1
numbers <- c(0:20)
if_else(numbers %% 2 == 0, "Even", "Odd")

#2
weekend <- c("Saturday", "Sunday")
vector1 <- c("Monday", "Saturday", "Wednesday")
if_else(vector1 %in% weekend, "Weekend", "Weekday")

#3
x <- c(-4, 6, 8, -10)
if_else(x < 0, -x, x)

#4
flights_with_holidays <- flights |>
  mutate(
    is_holiday = case_when(
      (month == 1 & day == 1) ~ TRUE,
      (month == 7 & day == 4) ~ TRUE,
      (month == 11 & day == 28) ~ TRUE,
      (month == 12 & day == 25) ~ TRUE,
      TRUE ~ FALSE),
    holiday_name = case_when(
      (month == 1 & day == 1) ~ "New Year's Day",
      (month == 7 & day == 4) ~ "Fourth of July",
      (month == 11 & day == 28) ~ "Thanksgiving",
      (month == 12 & day == 25) ~ "Christmas",
      TRUE ~ NA_character_)
  )


# Chapter 13 Exercises ----------------------------------------------------


# 13.3.1 ------------------------------------------------------------------
View(flights)
#2
#a
flights |>
  group_by(dest) |>
  summarize(
    count = n()) |>
  arrange(desc(count))

#b
flights |>
  group_by(tailnum) |>
  summarize(
    total_distance = sum(distance)) |>
  arrange(desc(total_distance))


# 13.4.8 ------------------------------------------------------------------
#2
#looked up in R help

#3
flights <- flights |>
  mutate(
    sched_dep_time = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100),
    dep_time = (dep_time %/% 100) * 60 + (dep_time %% 100)
  )

flights |> 
  filter(month == 1, day == 1) |> 
  ggplot(aes(x = sched_dep_time, y = dep_delay)) +
  geom_point()  

#4
flights <- flights |>
  mutate(
  dep_time_rounded = (dep_time %/% 100) * 100 + round((dep_time %% 100) / 5) * 5,
  arr_time_rounded = (arr_time %/% 100) * 100 + round((arr_time %% 100) / 5) * 5)



# 13.5.4 ------------------------------------------------------------------

#1
flights <- flights |>
  mutate(
    delay_rank = min_rank(desc(dep_delay))
  ) |>
  arrange(delay_rank)

#2
flights |>
  group_by(tailnum) |>
  summarize(
    on_time_record = mean(arr_delay, na.rm = TRUE)) |>
    mutate(
      on_time_rank = min_rank(desc(on_time_record))) |>
  arrange(on_time_rank)

#3
flights |>
  mutate(dep_hour = sched_dep_time %/% 100) |>
  group_by(dep_hour) |>
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) |>
  arrange(avg_dep_delay)

#4
flights |> 
  group_by(dest) |> 
  filter(row_number() < 4)

flights |> 
  group_by(dest) |> 
  filter(row_number(dep_delay) < 4)

#5
flights <- flights |>
  group_by(dest) |>
  mutate(
    total_delay = sum(arr_delay, na.rm = TRUE),
    prop_delay = arr_delay / total_delay)

#6
flights |> 
  mutate(hour = dep_time %/% 100) |> 
  group_by(year, month, day, hour) |> 
  summarize(
    dep_delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |> 
  filter(n > 5) |>
  mutate(prev_hour_delay = lag(dep_delay),
         delay_diff = prev_hour_delay - dep_delay)
  
#7
flights |>
  group_by(dest) |>
  mutate(
    shortest_air_time = min(air_time),
    relative_air_time = air_time / shortest_air_time) |>
  filter(relative_air_time > 1)

#8
distinct_carriers <- flights |>
  group_by(dest, carrier) |>
  summarize(num_flights = n()) |>
  filter(n_distinct(carrier) >= 2)

carrier_performance <- flights |>
  filter(dest %in% distinct_carriers$dest) |>
  group_by(dest, carrier) |>
  summarize(
    avg_dep_delay = mean(dep_delay, na.rm = TRUE)) |>
  arrange(dest, avg_dep_delay) |>
  mutate(rank = row_number())


# 13.6.7 ------------------------------------------------------------------

#2
flights |>
  group_by(dest) |>
  mutate(
    air_speed = distance / air_time) |>
  summarize(sd_air_speed = sd(air_time, na.rm = TRUE)) |>
  arrange(desc(sd_air_speed))

#3
ege_flights <- flights |>
  filter(dest == "EGE") 

plot_data <- ege_flights |>
  mutate(date = as.Date(paste(year, month, day, sep = "-")))

ggplot(plot_data, aes(x = date, y = air_time)) +
  geom_smooth()
