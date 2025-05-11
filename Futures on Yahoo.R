
url <- "https://finance.yahoo.com/markets/commodities/"
url

html <- read_html(url)
section <- html |>
  html_elements(".yf-12mq010")
section

html |>
  html_table()

table <- html |>
  html_element("table") |>
  html_table()

table



# Bonds -------------------------------------------------------------------

url <- "https://finance.yahoo.com/markets/bonds/"
url

html <- read_html(url)

section <- html |>
  html_elements(".yf-12mq010")
section

html |>
  html_table()

table <- html |>
  html_element("table") |>
  html_table()
table


# Options -----------------------------------------------------------------

url <- "https://finance.yahoo.com/markets/options/gainers/"
url

html <- read_html(url)

section <- html |>
  html_elements(".yf-j24h8w")
section

html |>
  html_table()

table <- html |>
  html_element("table") |>
  html_table()
table


# World Indices -----------------------------------------------------------

url <- "https://finance.yahoo.com/markets/world-indices/"
url

html <- read_html(url)

section <- html |>
  html_elements(".yf-j24h8w")
section

html |>
  html_table()

table <- html |>
  html_element("table") |>
  html_table()
table
