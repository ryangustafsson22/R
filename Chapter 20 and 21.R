
# Chapter 20 Executables --------------------------------------------------

#20.2 Excel
students <- read_excel("Data 332/students.xlsx")
students

students <- read_excel(
  "Data 332/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1
)

students <- read_excel(
  "Data 332/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1,
  na = c("", "N/A"),
  col_types = c("numeric", "text", "text", "text", "text")
)

students <- students |>
  mutate(
    age = if_else(age == "five", "5", age),
    age = parse_number(age)
  )
students

penguins_torgersen <- read_excel("Data 332/penguins.xlsx", sheet = "Torgersen Island", na = "NA")

penguins_torgersen

penguins_biscoe <- read_excel("Data 332/penguins.xlsx", sheet = "Biscoe Island", na = "NA")
penguins_dream  <- read_excel("Data 332/penguins.xlsx", sheet = "Dream Island", na = "NA")

dim(penguins_torgersen)
#> [1] 52  8
dim(penguins_biscoe)
#> [1] 168   8
dim(penguins_dream)
#> [1] 124   8

penguins <- bind_rows(penguins_torgersen, penguins_biscoe, penguins_dream)
penguins

deaths_path <- readxl_example("deaths.xlsx")
deaths <- read_excel(deaths_path)

read_excel(deaths_path, range = "A5:F15")

bake_sale <- tibble(
  item     = factor(c("brownie", "cupcake", "cookie")),
  quantity = c(10, 5, 8)
)

bake_sale

write_xlsx(bake_sale, path = "Data 332/bake-sale.xlsx")

read_excel("bake-sale.xlsx")

#20.3
gs4_deauth()
students_sheet_id <- "1V1nPp1tzOuutXFLb3G9Eyxi3qxeEhnOXUzL5_BcCQ0w"
students <- read_sheet(students_sheet_id)

students

students <- read_sheet(
  students_sheet_id,
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1,
  na = c("", "N/A"),
  col_types = "dcccc"
)

penguins_sheet_id <- "1aFu8lnD_g0yjF5O-K6SFgSEWiHPpgvFCF0NY9D6LXnY"
read_sheet(penguins_sheet_id, sheet = "Torgersen Island")

sheet_names(penguins_sheet_id)

deaths_url <- gs4_example("deaths")
deaths <- read_sheet(deaths_url, range = "A5:F15")

write_sheet(bake_sale, ss = "bake-sale")
write_sheet(bake_sale, ss = "bake-sale", sheet = "Sales")


# Chapter 21 Executables --------------------------------------------------

#21.3
con <- DBI::dbConnect(
  RMariaDB::MariaDB(), 
  username = "foo"
)
con <- DBI::dbConnect(
  RPostgres::Postgres(), 
  hostname = "databases.mycompany.com", 
  port = 1234
)

con <- DBI::dbConnect(duckdb::duckdb())
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "duckdb")

dbWriteTable(con, "mpg", ggplot2::mpg)
dbWriteTable(con, "diamonds", ggplot2::diamonds)

dbListTables(con)
#> [1] "diamonds" "mpg"

con |> 
  dbReadTable("diamonds") |> 
  as_tibble()

sql <- "
  SELECT carat, cut, clarity, color, price 
  FROM diamonds 
  WHERE price > 15000
"
as_tibble(dbGetQuery(con, sql))

#21.4
diamonds_db <- tbl(con, "diamonds")
diamonds_db

big_diamonds_db <- diamonds_db |> 
  filter(price > 15000) |> 
  select(carat:clarity, price)

big_diamonds_db

big_diamonds_db |>
  show_query()

big_diamonds <- big_diamonds_db |> 
  collect()
big_diamonds

#21.6
summarize_query <- function(df, ...) {
  df |> 
    summarize(...) |> 
    show_query()
}
mutate_query <- function(df, ...) {
  df |> 
    mutate(..., .keep = "none") |> 
    show_query()
}

flights |> 
  group_by(year, month, day) |>  
  summarize_query(
    mean = mean(arr_delay, na.rm = TRUE),
    median = median(arr_delay, na.rm = TRUE)
  )

flights |> 
  group_by(year, month, day) |>  
  mutate_query(
    mean = mean(arr_delay, na.rm = TRUE),
  )

flights |> 
  group_by(dest) |>  
  arrange(time_hour) |> 
  mutate_query(
    lead = lead(arr_delay),
    lag = lag(arr_delay)
  )


# Chapter 20 Exercises ----------------------------------------------------

#1
survey <- read_excel("survey.xlsx",
  col_names = c("survey_id", "n_pets"),
  col_types = c("text", "numeric"),
  skip = 1)

survey <- survey |>
  mutate(
    n_pets = if_else(n_pets == "two", "2", n_pets))
  
#2
roster <- read_excel("roster.xlsx",
                     col_names = c("group", "subgroup", "id"),
                     col_types = c("numeric", "text", "numeric"),
                     skip = 1)

roster <- roster |>
  fill(group, subgroup)

#3
sales <- read_excel("sales.xlsx",
                    col_names = c("id", "n"),
                    col_types = c("text", "text"),
                    skip = 3)

sales <- sales |>
  mutate(
    brand = if_else(grepl("^Brand", value), value, NA_character_)
  ) |>
  fill(brand) |>
  filter(!value %in% c("n", "Brand 1", "Brand 2"))

#couldn't figure this one out

#4
bake_sale <- tibble(
  item = c("Brownie", "Cupcake", "Cookie", "Muffin"),
  quantity = c(12, 18, 24, 10),
  price = c(1.5, 2.0, 1.0, 2.5))
  
write_xlsx(bake_sale, file = "bake_sale.xlsx")

#5
students_clean <- read_excel("students.xlsx")
students_clean <- students_clean |>
  clean_names()

#6
read_xls("students.xlsx")

#20.3.6
#1
read_excel("students.xlsx")

gs4_deauth()
students_sheet_id <- "1V1nPp1tzOuutXFLb3G9Eyxi3qxeEhnOXUzL5_BcCQ0w"
students <- read_sheet(students_sheet_id)

#Can't figure out why this code is giving me an error message
#Also gave me an error message in the executables
#Error in curl::curl_fetch_memory(url, handle = handle) : 
#Could not resolve hostname [sheets.googleapis.com]: Could not resolve host: sheets.googleapis.com

#2
url <- "https://docs.google.com/spreadsheets/d/1yc5gL-a2OOBr8M7B3IsDNX5uR17vBHOyWZq6xSTG2G8/edit?gid=0#gid=0"
survey <- read_sheet(url, col_types = "cd")

#3
url2 <- "https://docs.google.com/spreadsheets/d/1LgZ0Bkg9d_NK8uTdP2uHXm07kAlwx8-Ictf8NocebIE/edit?gid=0#gid=0"
roster <- read_sheet(url2, col_types = "dcd")
roster |>
  fill(group, subgroup)


# Chapter 21 Exercises ----------------------------------------------------

#21.5.10
#2
dbplyr::copy_nycflights13(con)
#> Creating table: airlines
#> Creating table: airports
#> Creating table: flights
#> Creating table: planes
#> Creating table: weather
flights <- tbl(con, "flights")
planes <- tbl(con, "planes")

flights |>
  filter(dep_delay < arr_delay)

flights |>
  mutate(speed = distance / (air_time / 60))
