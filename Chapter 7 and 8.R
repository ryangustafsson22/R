
# Chapter 7 ---------------------------------------------------------------

students <- read_csv("https://pos.it/r4ds-students-csv")

View(students)

students |> 
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`
  )

students |> janitor::clean_names()

#factor - known set of values, limited choices (Ex: Sm, M, L, XL)
students |>
  janitor::clean_names() |>
  mutate(meal_plan = factor(meal_plan))

students <- students |>
  janitor::clean_names() |>
  mutate(
    meal_plan = factor(meal_plan),
    age = parse_number(if_else(age == "five", "5", age))
  )

read_csv(
  "a,b,c
  1,2,3
  4,5,6"
)

read_csv(
  "The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3",
  skip = 2
)

read_csv(
  "# A comment I want to skip
  x,y,z
  1,2,3",
  comment = "#"
)

read_csv(
  "1,2,3
  4,5,6",
  col_names = FALSE
)

read_csv(
  "1,2,3
  4,5,6",
  col_names = c("x", "y", "z")
)

read_csv("
  logical,numeric,date,string
  TRUE,1,2021-01-15,abc
  false,4.5,2021-02-15,def
  T,Inf,2021-02-16,ghi
")

simple_csv <- "
  x
  10
  .
  20
  30"

read_csv(simple_csv)

df <- read_csv(
  simple_csv, 
  col_types = list(x = col_double())
)

problems(df)

read_csv(simple_csv, na = ".")

another_csv <- "
x,y,z
1,2,3"

read_csv(
  another_csv, 
  col_types = cols(.default = col_character())
)

read_csv(
  another_csv,
  col_types = cols_only(x = col_character())
)

sales_files <- c(
  "https://pos.it/r4ds-01-sales",
  "https://pos.it/r4ds-02-sales",
  "https://pos.it/r4ds-03-sales"
)
read_csv(sales_files, id = "file")

write_csv(students, "students.csv")

write_csv(students, "students-2.csv")
read_csv("students-2.csv")

write_rds(students, "students.rds")
read_rds("students.rds")

library(arrow)

write_parquet(students, "students.parquet")
read_parquet("students.parquet")

tibble(
  x = c(1, 2, 5), 
  y = c("h", "m", "g"),
  z = c(0.08, 0.83, 0.60)
)

tribble(
  ~x, ~y, ~z,
  1, "h", 0.08,
  2, "m", 0.83,
  5, "g", 0.60
)


# Chapter 8 ---------------------------------------------------------------

y <- 1:4
mean(y)

reprex::reprex()



# Chapter 7 Exercises -----------------------------------------------------

#7.2.4
#5
read_csv("a,b\n1,2,3\n4,5,6")
read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a,b\n\"1")
read_csv("a,b\n1,2\na,b")
read_csv("a;b\n1;3")

#6
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
annoying |>
  select('1')

ggplot(
  data = annoying,
  mapping = aes(x = `1`, y = `2`)) +
  geom_point()

annoying <- annoying |>
  mutate('3' = `2` / `1`)

annoying |>
  rename(one = `1`, two = `2`, three = `3`)


