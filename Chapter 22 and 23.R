library(dbplyr, warn.conflicts = FALSE)
library(duckdb)


# Chapter 22 Executables --------------------------------------------------

#22.3
dir.create("data", showWarnings = FALSE)

curl::multi_download(
  "https://r4ds.s3.us-west-2.amazonaws.com/seattle-library-checkouts.csv",
  "data/seattle-library-checkouts.csv",
  resume = TRUE
)

seattle_csv <- open_dataset(
  sources = "Data 332/seattle-library-checkouts.csv", 
  col_types = schema(ISBN = string()),
  format = "csv"
)

seattle_csv |> glimpse()

seattle_csv |> 
  group_by(CheckoutYear) |> 
  summarise(Checkouts = sum(Checkouts)) |> 
  arrange(CheckoutYear) |> 
  collect()

#22.4
pq_path <- "data/seattle-library-checkouts"
seattle_csv |>
  group_by(CheckoutYear) |>
  write_dataset(path = pq_path, format = "parquet")

tibble(
  files = list.files(pq_path, recursive = TRUE),
  size_MB = file.size(file.path(pq_path, files)) / 1024^2
)

#22.5
seattle_pq <- open_dataset(pq_path)

query <- seattle_pq |> 
  filter(CheckoutYear >= 2018, MaterialType == "BOOK") |>
  group_by(CheckoutYear, CheckoutMonth) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(CheckoutYear, CheckoutMonth)

query
query |>
  collect()

seattle_csv |> 
  filter(CheckoutYear == 2021, MaterialType == "BOOK") |>
  group_by(CheckoutMonth) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutMonth)) |>
  collect() |> 
  system.time()

seattle_pq |> 
  filter(CheckoutYear == 2021, MaterialType == "BOOK") |>
  group_by(CheckoutMonth) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutMonth)) |>
  collect() |> 
  system.time()

seattle_pq |> 
  to_duckdb() |>
  filter(CheckoutYear >= 2018, MaterialType == "BOOK") |>
  group_by(CheckoutYear) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutYear)) |>
  collect()


# Chapter 23 Executables --------------------------------------------------

#23.2 Lists
x1 <- list(1:4, "a", TRUE)
x1

x2 <- list(a = 1:2, b = 1:3, c = 1:4)
x2

str(x1)
str(x2)

x3 <- list(list(1, 2), list(3, 4))
str(x3)

x4 <- c(list(1, 2), list(3, 4))
str(x4)

x5 <- list(1, list(2, list(3, list(4, list(5)))))
str(x5)

df <- tibble(
  x = 1:2, 
  y = c("a", "b"),
  z = list(list(1, 2), list(3, 4, 5))
)
df

df |> 
  filter(x == 1)

#23.3 Unnesting
df1 <- tribble(
  ~x, ~y,
  1, list(a = 11, b = 12),
  2, list(a = 21, b = 22),
  3, list(a = 31, b = 32),
)
df1


df2 <- tribble(
  ~x, ~y,
  1, list(11, 12, 13),
  2, list(21),
  3, list(31, 32),
)
df2

df1 |> 
  unnest_wider(y)
df1 |> 
  unnest_wider(y, names_sep = "_")

df2 |> 
  unnest_longer(y)

df6 <- tribble(
  ~x, ~y,
  "a", list(1, 2),
  "b", list(3),
  "c", list()
)
df6 |> unnest_longer(y)
df6 |> unnest_longer(y, keep_empty = TRUE)

df4 <- tribble(
  ~x, ~y,
  "a", list(1),
  "b", list("a", TRUE, 5)
)
df4 |> 
  unnest_longer(y)

#23.4 Case Studies
repos <- tibble(json = gh_repos)
repos

repos <- repos |> 
  unnest_longer(json)

repos <- repos |> 
  unnest_longer(json) |> 
  unnest_wider(json, names_repair = "unique")

repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  names() |> 
  head(10)

repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  select(id, full_name, owner, description)

repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  select(id, full_name, owner, description) |> 
  unnest_wider(owner)

repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  select(id, full_name, owner, description) |> 
  unnest_wider(owner, names_sep = "_")

chars <- tibble(json = got_chars)
chars

chars <- chars |> 
  unnest_wider(json)

characters <- chars |> 
  unnest_wider(json) |> 
  select(id, name, gender, culture, born, died, alive)
characters

chars |> 
  unnest_wider(json) |> 
  select(id, where(is.list))

chars |> 
  unnest_wider(json) |> 
  select(id, titles) |> 
  unnest_longer(titles)

titles <- chars |> 
  unnest_wider(json) |> 
  select(id, titles) |> 
  unnest_longer(titles) |> 
  filter(titles != "") |> 
  rename(title = titles)
titles

gmaps_cities
gmaps_cities <- gmaps_cities |> 
  unnest_wider(json)

gmaps_cities |> 
  unnest_wider(json) |> 
  select(-status) |> 
  unnest_longer(results)

locations <- gmaps_cities |> 
  unnest_wider(json) |> 
  select(-status) |> 
  unnest_longer(results) |> 
  unnest_wider(results)
locations

locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry)

locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry) |> 
  unnest_wider(location)

locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry) |> 
  # focus on the variables of interest
  select(!location:viewport) |>
  unnest_wider(bounds)

locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry) |> 
  select(!location:viewport) |>
  unnest_wider(bounds) |> 
  rename(ne = northeast, sw = southwest) |> 
  unnest_wider(c(ne, sw), names_sep = "_") 

locations |> 
  select(city, formatted_address, geometry) |> 
  hoist(
    geometry,
    ne_lat = c("bounds", "northeast", "lat"),
    sw_lat = c("bounds", "southwest", "lat"),
    ne_lng = c("bounds", "northeast", "lng"),
    sw_lng = c("bounds", "southwest", "lng"),
  )

#23.5 Json
# A path to a json file inside the package:
gh_users_json()
#> [1] "/home/runner/work/_temp/Library/repurrrsive/extdata/gh_users.json"

# Read it with read_json()
gh_users2 <- read_json(gh_users_json())

# Check it's the same as the data we were using previously
identical(gh_users, gh_users2)
#> [1] TRUE

str(parse_json('1'))
#>  int 1
str(parse_json('[1, 2, 3]'))
#> List of 3
#>  $ : int 1
#>  $ : int 2
#>  $ : int 3
str(parse_json('{"x": [1, 2, 3]}'))
#> List of 1
#>  $ x:List of 3
#>   ..$ : int 1
#>   ..$ : int 2
#>   ..$ : int 3

json <- '[
  {"name": "John", "age": 34},
  {"name": "Susan", "age": 27}
]'
df <- tibble(json = parse_json(json))
df

df |> 
  unnest_wider(json)

json <- '{
  "status": "OK", 
  "results": [
    {"name": "John", "age": 34},
    {"name": "Susan", "age": 27}
 ]
}
'
df <- tibble(json = list(parse_json(json)))
df

df |> 
  unnest_wider(json) |> 
  unnest_longer(results) |> 
  unnest_wider(results)

df <- tibble(results = parse_json(json)$results)
df |> 
  unnest_wider(results)


# Chapter 22 Exercises ----------------------------------------------------


#My computer doesn't have enough storage to download the database,
#but I believe this is the right code for these exercises
#1
seattle_pq |> 
  to_duckdb() |>
  filter(CheckoutYear >= 2018, MaterialType == "BOOK") |>
  group_by(CheckoutYear, Title) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(CheckoutYear, desc(TotalCheckouts)) |>
  collect(

#2
seattle_pq |>
  to_duckdb() |>
  filter(MaterialType == "BOOK") |>
  group_by(Author, Title) |>
  summarize(TotalBooks = sum(Title)) |>
  arrange(desc(TotalBooks)) |>
  collect()

#3
seattle_pq() |>
  to_duckdb() |>
  filter(CheckoutYear >= 2015, MaterialType %in% c("BOOK", "EBOOK")) |>
  group_by(CheckoutYear, MaterialType) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutYear))


# Chapter 23 Exercises ----------------------------------------------------

#23.3.5
#1
df2 <- tribble(
  ~x, ~y,
  1, list(11, 12, 13),
  2, list(21),
  3, list(31, 32),
)

df2 |>
  unnest_wider(y)

#2
df1 <- tribble(
  ~x, ~y,
  1, list(a = 11, b = 12),
  2, list(a = 21, b = 22),
  3, list(a = 31, b = 32),
)

df1 |>
  unnest_longer(y) |>
  select(-y_id)

#3
df4 <- tribble(
  ~x, ~y, ~z,
  "a", list("y-a-1", "y-a-2"), list("z-a-1", "z-a-2"),
  "b", list("y-b-1", "y-b-2", "y-b-3"), list("z-b-1", "z-b-2", "z-b-3")
)

df4 |>
  unnest_longer(y) |>
  unnest_longer(z)

#23.4.5

#the code in the book is not allowing me to unnest the columns in gh_repos
#since it says the lists are unnamed. I will try to write the code to the best of my
#ability. 

#1
repos |>
  names()
#could not solve due to not having the correct columns in the dataset
#2
owners <- repos |>
  unnest_longer(owner) |>
  distinct(owner)

#3
aliases <- chars |>
  select(id, aliases) |>
  unnest_longer(aliases) |>
  filter(aliases != "") |>
  rename(alias = aliases)

allegiances <- chars |>
  select(id, allegiances) |>
  unnest_longer(allegiances) |>
  filter(allegiances != "") |>
  rename(allegiance = allegiances)

books <- chars |> 
  select(id, books) |> 
  unnest_longer(books) |> 
  filter(books != "") |>
  rename(book = books)

tvSeries <- chars |> 
  select(id, tvSeries) |> 
  unnest_longer(tvSeries) |> 
  filter(tvSeries != "")

#5
gmaps_cities |>
  unnest_longer(results) |>
  unnest_wider(results) |>
  unnest_longer(address_components) |>
  unnest_wider(address_components)

#23.5.4
#1
json_col <- parse_json('
  {
    "x": ["a", "x", "z"],
    "y": [10, null, 3]
  }
')
json_row <- parse_json('
  [
    {"x": "a", "y": 10},
    {"x": "x", "y": null},
    {"x": "z", "y": 3}
  ]
')

df_col <- tibble(json = list(json_col)) 
df_row <- tibble(json = json_row)

df_col <- df_col |>
  unnest_wider(json)
df_row <- df_row |>
  unnest_wider(json)