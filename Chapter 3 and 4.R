# Chapter 3 and 4 Executables

# Chapter 3
library(nycflights13)
library(tidyverse)

?flights

flights |>
  filter(dest == "IAH") |> 
  group_by(year, month, day) |> 
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

flights |> 
  filter(dep_delay > 120)

flights |> 
  filter(month == 1 & day == 1)

flights |> 
  filter(month == 1 | month == 2)

flights |> 
  filter(month == 1 | 2)

flights |> 
  filter(month %in% c(1, 2))

#Dimension --> Measure: value given to the dimension
jan1 <- flights |> 
  filter(month == 1 & day == 1)

flights |> 
  arrange(year, month, day, dep_time)

flights |> 
  arrange(desc(dep_delay))

flights |> 
  distinct()

flights |> 
  distinct(origin, dest)

flights |> 
  distinct(origin, dest, .keep_all = TRUE)

flights |>
  count(origin, dest, sort = TRUE)

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60
  )

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  )

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .after = day
  )

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )

flights |> 
  select(where(is.character))

flights |> 
  rename(tail_num = tailnum)

flights |> 
  relocate(time_hour, air_time)

flights |> 
  relocate(year:dep_time, .after = time_hour)
flights |> 
  relocate(starts_with("arr"), .before = dep_time)

flights |> 
  filter(dest == "IAH") |> 
  mutate(speed = distance / air_time * 60) |> 
  select(year:day, dep_time, carrier, flight, speed) |> 
  arrange(desc(speed))

arrange(
  select(
    mutate(
      filter(
        flights, 
        dest == "IAH"
      ),
      speed = distance / air_time * 60
    ),
    year:day, dep_time, carrier, flight, speed
  ),
  desc(speed)
)

flights1 <- filter(flights, dest == "IAH")
flights2 <- mutate(flights1, speed = distance / air_time * 60)
flights3 <- select(flights2, year:day, dep_time, carrier, flight, speed)
arrange(flights3, desc(speed))

flights |> 
  group_by(month)

flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay)
  )

flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE)
  )

flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    n = n()
  )

flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 1) |>
  relocate(dest)
# with_ties = FALSE to get rid of duplicate maxes

daily <- flights |>  
  group_by(year, month, day)
daily

daily_flights <- daily |> 
  summarize(n = n())

daily_flights <- daily |> 
  summarize(
    n = n(), 
    .groups = "drop_last"
  )

daily |> 
  ungroup()

daily |> 
  ungroup() |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    flights = n()
  )

flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = month
  )

flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = c(origin, dest)
  )

# Chapter 4 ---------------------------------------------------------------


short_flights <- flights |> filter(air_time < 60)


mean(x, na.rm = TRUE)

flights |> 
  mutate(
    speed      = distance / air_time,
    dep_hour   = dep_time %/% 100,
    dep_minute = dep_time %%  100
  )

#Checks if arr_delay is na or tailnum is na
flights |>  
  filter(!is.na(arr_delay), !is.na(tailnum)) |> 
  count(dest)

flights |>  
  group_by(tailnum) |> 
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

flights |> 
  group_by(month) |> 
  summarize(
    delay = mean(arr_delay, na.rm = TRUE)
  ) |> 
  ggplot(aes(x = month, y = delay)) +
  geom_point() + 
  geom_line()

flights |> 
  group_by(dest) |> 
  summarize(
    distance = mean(distance),
    speed = mean(distance / air_time, na.rm = TRUE)
  ) |> 
  ggplot(aes(x = distance, y = speed)) +
  geom_smooth(
    method = "loess",
    span = 0.5,
    se = FALSE, 
    color = "white", 
    linewidth = 4
  ) +
  geom_point()


# Chapter 3 and 4 Exercises -----------------------------------------------

#Chapter 3.2.5 Exercises
#1
flights |> 
  filter(arr_delay > 120)
         
flights |>
  filter(dest == "IAH")
         
flights |>
  filter(carrier %in% c("UA", "DA", "DL"))

flights |>
  filter(month %in% c(7, 8, 9))

flights |>
  filter(dep_delay < 1 & arr_delay > 120)

flights |>
  filter(dep_delay >= 60 & arr_delay - dep_delay > 30)

#2
flights |>
  arrange(desc(dep_delay), sched_dep_time)

#3
flights |>
  arrange(arr_time - dep_time)

#4
flights |>
  filter(year == 2013) |>
  distinct(month, day)
  
#5
flights |>
  arrange(desc(distance)) |>
  select(flight, distance) |>
  distinct(flight, distance)
flights |>
  arrange(distance) |>
  select(flight, distance) |>
  distinct(flight, distance)

# 3.3.5 Exercises
#1
flights |>
  select(dep_time, sched_dep_time, dep_delay)

#2
flights |> 
  select(dep_time, dep_delay, arr_time, arr_delay)

flights |>
  select(starts_with("dep"), starts_with("arr"))

flights |>
  select(ends_with("time"), ends_with("delay")) |>
  select(starts_with("arr"), starts_with("dep"))

#3
flights |>
  select(dep_time, arr_time, dep_time)

#4
variables <- c("year", "month", "day", "dep_delay", "arr_delay")
flights |>
  select(any_of(variables))

#5
flights |> select(contains("TIME"))

#6
flights |>
  rename(air_time_min = air_time) |>
  relocate(air_time_min, .before = year)

#7
flights |> 
  select(tailnum) |> 
  arrange(arr_delay)

#Chapter 3.5.7 Exercises
#1
flights |>
  group_by(carrier, dest) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE) ) |>
  arrange(desc(avg_delay))

#2
flights |>
  select(origin, flight, dep_delay, carrier) |>
  group_by(origin) |>
  slice_max(dep_delay, n = 1)
  
#3
flights |>
ggplot(
  data = flights,
  mapping = aes(x = sched_dep_time, y = dep_delay)
) +
  geom_point()

#4
flights |>
  select(origin, flight, dep_delay, carrier) |>
  group_by(origin) |>
  slice_max(dep_delay, -n = 1)

#6
#A
df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)
df |>
  group_by(y)
#B
df |>
  arrange(y)
#C
df |>
  group_by(y) |>
  summarize(mean_x = mean(x))
#D
df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))

#E
df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x), .groups = "drop")
#F
df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))
df |>
  group_by(y, z) |>
  mutate(mean_x = mean(x))

#Chapter 4.6 Exercises
flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(n = n(), delay = mean(arr_delay, na.rm = TRUE)) |>
  filter(n > 10)
                                                      

flights |>
  filter(carrier == "UA", dest %in% c("IAH", "HOU"), sched_dep_time > 0900, sched_arr_time < 2000) |>
  group_by(flight) |>
  summarize(delay = mean(arr_delay, na.rm = TRUE), cancelled = sum(is.na(arr_delay)), n = n()) |>
  filter(n>10)
