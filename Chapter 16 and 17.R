
# Chapter 16 Executables --------------------------------------------------

#16.2 Factor Basics
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")
sort(x1)

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

y1 <- factor(x1, levels = month_levels)
y1
sort(y1)

y2 <- factor(x2, levels = month_levels)
y2

y2 <- fct(x2, levels = month_levels)

fct(x1)

levels(y2)

csv <- "
month,value
Jan,12
Feb,56
Mar,12"

df <- read_csv(csv, col_types = cols(month = col_factor(month_levels)))
df$month

#16.3 General Social Survey
gss_cat |>
  count(race)

#16.4 Modifying Factor Order
relig_summary <- gss_cat |>
  group_by(relig) |>
  summarize(
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig_summary, aes(x = tvhours, y = relig)) +
  geom_point()

ggplot(relig_summary, aes(x = tvhours, y = fct_reorder(relig, tvhours))) +
  geom_point()

relig_summary |>
  mutate(
    relig = fct_reorder(relig, tvhours)
  ) |>
  ggplot(aes(x = tvhours, y = relig)) +
  geom_point()

rincome_summary <- gss_cat |>
  group_by(rincome) |>
  summarize(
    age = mean(age, na.rm = TRUE),
    n = n()
  )
ggplot(rincome_summary, aes(x = age, y = fct_reorder(rincome, age))) +
  geom_point()

ggplot(rincome_summary, aes(x = age, y = fct_relevel(rincome, "Not applicable"))) +
  geom_point()


by_age <- gss_cat |>
  filter(!is.na(age)) |>
  count(age, marital) |>
  group_by(age) |>
  mutate(
    prop = n / sum(n)
  )

ggplot(by_age, aes(x = age, y = prop, color = marital)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1")

ggplot(by_age, aes(x = age, y = prop, color = fct_reorder2(marital, age, prop))) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "marital")

gss_cat |>
  mutate(marital = marital |> fct_infreq() |> fct_rev()) |>
  ggplot(aes(x = marital)) +
  geom_bar()
#fct_infreq() orders in decreasing frequency
#fct_rev() puts largest values on the right

#16.5 Modifying Factor Levels

gss_cat |> count(partyid)

gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat"
    )
  ) |>
  count(partyid)

gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat",
                         "Other"                 = "No answer",
                         "Other"                 = "Don't know",
                         "Other"                 = "Other party"
    )
  )

gss_cat |>
  mutate(
    partyid = fct_collapse(partyid,
                           "other" = c("No answer", "Don't know", "Other party"),
                           "rep" = c("Strong republican", "Not str republican"),
                           "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
                           "dem" = c("Not str democrat", "Strong democrat")
    )
  ) |>
  count(partyid)

gss_cat |>
  mutate(relig = fct_lump_lowfreq(relig)) |>
  count(relig)

gss_cat |>
  mutate(relig = fct_lump_n(relig, n = 10)) |>
  count(relig, sort = TRUE)

#16.6 Ordered Factors
ordered(c("a", "b", "c"))


# Chapter 17 Executables --------------------------------------------------

#17.2 Creating date/times
today()
now()

csv <- "
  date,datetime
  2022-01-02,2022-01-02 05:12
"
read_csv(csv)

csv <- "
  date
  01/02/15
"

read_csv(csv, col_types = cols(date = col_date("%m/%d/%y")))
#> # A tibble: 1 × 1
#>   date      
#>   <date>    
#> 1 2015-01-02

read_csv(csv, col_types = cols(date = col_date("%d/%m/%y")))
#> # A tibble: 1 × 1
#>   date      
#>   <date>    
#> 1 2015-02-01

read_csv(csv, col_types = cols(date = col_date("%y/%m/%d")))
#> # A tibble: 1 × 1
#>   date      
#>   <date>    
#> 1 2001-02-15
#> 

ymd("2017-01-31")
#> [1] "2017-01-31"
mdy("January 31st, 2017")
#> [1] "2017-01-31"
dmy("31-Jan-2017")
#> [1] "2017-01-31"

ymd_hms("2017-01-31 20:11:59")
#> [1] "2017-01-31 20:11:59 UTC"
mdy_hm("01/31/2017 08:01")
#> [1] "2017-01-31 08:01:00 UTC"

ymd("2017-01-31", tz = "UTC")
#> [1] "2017-01-31 UTC"

flights |> 
  select(year, month, day, hour, minute)

flights |> 
  select(year, month, day, hour, minute) |> 
  mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights |> 
  filter(!is.na(dep_time), !is.na(arr_time)) |> 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) |> 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt

flights_dt |> 
  ggplot(aes(x = dep_time)) + 
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt |> 
  filter(dep_time < ymd(20130102)) |> 
  ggplot(aes(x = dep_time)) + 
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

as_datetime(today())

as_date(now())

#17.3 Date-Time Components
datetime <- ymd_hms("2026-07-08 12:34:56")

year(datetime)
#> [1] 2026
month(datetime)
#> [1] 7
mday(datetime)
#> [1] 8

yday(datetime)
#> [1] 189
wday(datetime)
#> [1] 4

month(datetime, label = TRUE)
wday(datetime, label = TRUE, abbr = FALSE)

flights_dt |> 
  mutate(wday = wday(dep_time, label = TRUE)) |> 
  ggplot(aes(x = wday)) +
  geom_bar()

flights_dt |> 
  mutate(minute = minute(dep_time)) |> 
  group_by(minute) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  ) |> 
  ggplot(aes(x = minute, y = avg_delay)) +
  geom_line()

sched_dep <- flights_dt |> 
  mutate(minute = minute(sched_dep_time)) |> 
  group_by(minute) |> 
  summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
ggplot(sched_dep, aes(x = minute, y = avg_delay)) +
  geom_line()


flights_dt |> 
  count(week = floor_date(dep_time, "week")) |> 
  ggplot(aes(x = week, y = n)) +
  geom_line() + 
  geom_point()

flights_dt |> 
  mutate(dep_hour = dep_time - floor_date(dep_time, "day")) |> 
  ggplot(aes(x = dep_hour)) +
  geom_freqpoly(binwidth = 60 * 30)

flights_dt |> 
  mutate(dep_hour = hms::as_hms(dep_time - floor_date(dep_time, "day"))) |> 
  ggplot(aes(x = dep_hour)) +
  geom_freqpoly(binwidth = 60 * 30)


(datetime <- ymd_hms("2026-07-08 12:34:56"))
#> [1] "2026-07-08 12:34:56 UTC"

year(datetime) <- 2030
datetime
#> [1] "2030-07-08 12:34:56 UTC"
month(datetime) <- 01
datetime
#> [1] "2030-01-08 12:34:56 UTC"
hour(datetime) <- hour(datetime) + 1
datetime
#> [1] "2030-01-08 13:34:56 UTC"

update(datetime, year = 2030, month = 2, mday = 2, hour = 2)

update(ymd("2023-02-01"), mday = 30)
#> [1] "2023-03-02"
update(ymd("2023-02-01"), hour = 400)
#> [1] "2023-02-17 16:00:00 UTC"


#17.4 Time Spans
# How old is Hadley?
h_age <- today() - ymd("1979-10-14")
h_age
#> Time difference of 16619 days

as.duration(h_age)

dseconds(15)
#> [1] "15s"
dminutes(10)
#> [1] "600s (~10 minutes)"
dhours(c(12, 24))
#> [1] "43200s (~12 hours)" "86400s (~1 days)"
ddays(0:5)
#> [1] "0s"                "86400s (~1 days)"  "172800s (~2 days)"
#> [4] "259200s (~3 days)" "345600s (~4 days)" "432000s (~5 days)"
dweeks(3)
#> [1] "1814400s (~3 weeks)"
dyears(1)
#> [1] "31557600s (~1 years)"


# Chapter 16 Exercises ----------------------------------------------------

#16.3.1
#1
ggplot(gss_cat, aes(x = fct_relevel(rincome, "Not applicable"))) +
  geom_bar() +
  coord_flip()

#2
gss_cat |>
  count(relig)

gss_cat |>
  count(partyid)

#3
gss_cat |>
  count(relig, denom) |>
  filter(!denom %in% c("Don't know", "Not applicable", "No answer", "No denomination"))

gss_cat |>
  count(relig, denom) |>
  filter(!denom %in% c("Don't know", "Not applicable", "No answer", "No denomination")) |>
  ggplot(aes(x = relig, y = denom, fill = n)) +
  geom_point()

#16.4.1
#1
gss_cat |>
  filter(tvhours > 12)

#16.5.1
#1
threeparties <- gss_cat |>
  mutate(
    partyid = fct_collapse(partyid,
                           "other" = c("No answer", "Don't know", "Other party"),
                           "rep" = c("Strong republican", "Not str republican"),
                           "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
                           "dem" = c("Not str democrat", "Strong democrat")
    )) |>
  count(year, partyid) |>
  group_by(year) |>
  mutate(prop = n / sum(n))

threeparties |>
  ggplot(aes(x = year, y = prop, color = partyid)) +
  geom_line(linewidth = 1)

#2
gss_cat |>
  mutate(
    rincome = fct_collapse(rincome,
                           "low" = c("Lt $1000", "$1000 to 2999", "$3000 to 3999",
                                     "$4000 to 4999", "$5000 to 5999", "$6000 to 6999",
                                     "$7000 to 7999", "$8000 to 9999"),
                           "middle" = c("$10000 - 14999", "$15000 - 19999", "$20000 - 24999"),
                           "high" = c("$25000 or more"),
                           "Other" = c("Refused", "Don't know", "No answer", "Not applicable"))
  )

#3
fct_lump


# Chapter 17 Exercises ----------------------------------------------------

#17.2.5
#1
ymd(c("2010-10-10", "bananas"))

#2
today(tzone = "UTC")

#3
d1 <- "January 1, 2010"
read_csv("date\nJanuary 1, 2010", 
               col_types = cols(date = col_date(format = "%B %d, %Y")))
mdy(c(d1))
d2 <- "2015-Mar-07"
read_csv("date/n2015-Mar-07",
         col_types = cols(date = col_date(format = "%Y-%b-%d")))
ymd(c(d2))
d3 <- "06-Jun-2017"
read_csv("date/n06-Jun-2017",
         col_types = cols(date = col_date(format = "%d-%b-%Y")))
dmy(c(d3))
d4 <- c("August 19 (2015)", "July 1 (2015)")
read_csv("date/nAugust 19 (2015)", 
         col_types = cols(date = col_date(format = "%B %d (%Y)")))
parse_date_time(d4, "B d (Y)")
d5 <- "12/30/14"
read_csv("date/n12/30/14",
         col_types = cols(date = col_date(format = "%m/%d/%y")))
mdy(c(d5))
t1 <- "1705"
hm(t1)
t2 <- "11:15:10.12 PM"
parse_time(t2, "%I:%M:%OS %p")

#17.3.4
#1
cleaned_flights <- flights |>
  filter(!is.na(dep_time)) |>
  mutate(
    dep_hour = dep_time %/% 100,
    dep_minute = dep_time %% 100,
    dep_time_parsed = make_datetime(year, month, day, dep_hour, dep_minute))

cleaned_flights |>
  mutate(
    hour = hour(dep_time_parsed),
    month = month(dep_time_parsed)) |>
  ggplot(aes(x = hour)) +
  geom_density(fill = "blue", alpha = 0.6) +
  facet_wrap(~ month, ncol = 3)

#2
flights_comparison <- flights |>
  mutate(
    sched_dep_minutes = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100),
    dep_minutes = (dep_time %/% 100) * 60 + (dep_time %% 100),
    delay_calc = sched_dep_minutes - dep_minutes)

flights_comparison |>
  mutate(diff = dep_delay - delay_calc) |>
  count(diff) |>
  arrange(desc(n))

#3
flights_air_time <- flights |>
  mutate(
    dep_minutes = (dep_time %/% 100) * 60 + (dep_time %% 100),
    arr_minutes = (arr_time %/% 100) * 60 + (arr_time %% 100),
    flight_duration = arr_minutes - dep_minutes,
    duration_diff = flight_duration - air_time)

flights_air_time |>
  select(dest, dep_time, arr_time, air_time, flight_duration, duration_diff)

#4
flights_adjusted <- flights |>
  mutate(
    sched_hour = (sched_dep_time %/% 100)
  )

flights_adjusted |>
  group_by(sched_hour) |>
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) |>
  ggplot(aes(x = sched_hour, y = avg_delay)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "blue")

#5
flights |>
  mutate(
    date = make_date(year, month, day),
    weekday = wday(date, label = TRUE, abbr = FALSE)) |>
  group_by(weekday) |>
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) |>
  arrange(avg_delay)

#7
flights_clean <- flights |>
  mutate(
    dep_min = dep_time %% 100,
    sched_min = sched_dep_time %% 100,
    was_delayed = dep_delay > 0)

flights_clean |>
  filter((dep_min >= 20 & dep_min < 31) | (dep_min >= 50 & dep_min <= 59))

#couldn't figure this one out

#17.4.4
#1
days(!overnight)

#2
first_of_month_2015 <- ymd(paste0("2015-", 1:12, "-01"))
first_of_month_2015

current_year <- year(today())
first_of_month_current <- ymd(paste0(current_year, "-", 1:12, "-01" ))
first_of_month_current

#3
birthday <- "2004-06-15"
birthday <- as_date(birthday)
today <- today()

age <- interval(birthday, today) / years(1)
age
floor(age)

#4
(today() %--% (today() + years(1))) / months(1)
