
# Chapter 14 Executables --------------------------------------------------

#14.2

string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'

double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"

backslash <- "\\"

x <- c(single_quote, double_quote, backslash)
x
str_view(x)

tricky <- "double_quote <- \"\\\"\" # or '\"'
single_quote <- '\\'' # or \"'\""
str_view(tricky)

tricky <- r"(double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'")"
str_view(tricky)

x <- c("one\ntwo", "one\ttwo", "\u00b5", "\U0001f604")
x

#14.3

str_c("x", "y")

df <- tibble(name = c("Flora", "David", "Terra", NA))
df |> mutate(greeting = str_c("Hi ", name, "!"))

df |> 
  mutate(
    greeting1 = str_c("Hi ", coalesce(name, "you"), "!"),
    greeting2 = coalesce(str_c("Hi ", name, "!"), "Hi!")
  )

df |> mutate(greeting = str_glue("Hi {name}!"))

df |> mutate(greeting = str_glue("{{Hi {name}!}}"))

str_flatten(c("x", "y", "z"))
str_flatten(c("x", "y", "z"), ", ")
str_flatten(c("x", "y", "z"), ", ", last = ", and ")

df <- tribble(
  ~ name, ~ fruit,
  "Carmen", "banana",
  "Carmen", "apple",
  "Marvin", "nectarine",
  "Terence", "cantaloupe",
  "Terence", "papaya",
  "Terence", "mandarin"
)

df |>
  group_by(name) |> 
  summarize(fruits = str_flatten(fruit, ", "))

#14.4

df1 <- tibble(x = c("a,b,c", "d,e", "f"))
df1 |> 
  separate_longer_delim(x, delim = ",")

df2 <- tibble(x = c("1211", "131", "21"))
df2 |> 
  separate_longer_position(x, width = 1)

df3 <- tibble(x = c("a10.1.2022", "b10.2.2011", "e15.1.2015"))
df3 |> 
  separate_wider_delim(
    x,
    delim = ".",
    names = c("code", "edition", "year")
  )

df3 |> 
  separate_wider_delim(
    x,
    delim = ".",
    names = c("code", NA, "year")
  )

df4 <- tibble(x = c("202215TX", "202122LA", "202325CA")) 
df4 |> 
  separate_wider_position(
    x,
    widths = c(year = 4, age = 2, state = 2)
  )

df <- tibble(x = c("1-1-1", "1-1-2", "1-3", "1-3-2", "1"))

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z")
  )

debug <- df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_few = "debug"
  )
debug
debug |>
  filter(!x_ok)

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_few = "align_start"
  )

df <- tibble(x = c("1-1-1", "1-1-2", "1-3-5-6", "1-3-2", "1-3-5-7-9"))

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z")
  )

debug <- df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "debug"
  )
debug

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "drop"
  )

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "merge"
  )

#14.5

str_length(c("a", "R for data science", NA))

babynames |>
  count(length = str_length(name), wt = n)

babynames |> 
  filter(str_length(name) == 15) |> 
  count(name, wt = n, sort = TRUE)

x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)

str_sub(x, -3, -1)

str_sub("a", 1, 5)

babynames |> 
  mutate(
    first = str_sub(name, 1, 1),
    last = str_sub(name, -1, -1)
  )

charToRaw("Hadley")

x1 <- "text\nEl Ni\xf1o was particularly bad this year"
read_csv(x1)$text

read_csv(x1, locale = locale(encoding = "Latin1"))$text

u <- c("\u00fc", "u\u0308")
str_view(u)

u[[1]] == u[[2]]
str_equal(u[[1]], u[[2]])

str_to_upper(c("i", "ı"))

str_to_upper(c("i", "ı"), locale = "tr")


# Chapter 14 Exercises ----------------------------------------------------

#14.2.4 Exercises
#1
string1 = 'He said "That\'s Amazing!"'
str_view(string1)

string2 = "\\a\\b\\c\\d"
str_view(string2)

string3 <- "\\\\\\\\"
str_view(string3)

#2
x <- "This\u00a0is\u00a0tricky"
print(x)
str_view(x)

#14.3.4 Exercises
#1
str_c("hi ", NA)
str_c(letters[1:2], letters[1:3])

paste0("hi ", NA)
paste0(letters[1:2], letters[1:3])

#2
paste("hi ", NA)

#3
str_glue("The price of {food} is {price}")

str_c("I\'m", age, "years old and live in", country)

title <- "Hello"
str_glue("\\\\section{{{title}}}")

#14.5.3
#1
babynames |>
  count(length = str_length(name), wt = n)

babynames |>
  count(length = str_length(name))

#2
babynames |>
  mutate(middle_letter = round(str_length(name) / 2),
         middle_letter = str_sub(name, middle_letter, middle_letter))

#3
#a
babynames <- babynames |> 
  mutate(name_length = str_length(name))

avg_name_length <- babynames |>
  group_by(year) |>
  summarize(avg_length = mean(name_length))

ggplot(avg_name_length, aes(x = year, y = avg_length)) +
  geom_line()

babynames <- babynames |>
  mutate(first_letter = str_sub(name, 1, 1),
         last_letter = str_sub(name, -1, -1))

babynames |>
  group_by(year)
#couldn't quite figure out how to do the second part of #3


# Chapter 15 Executables --------------------------------------------------

str_view(fruit, "berry")

str_view(c("a", "ab", "ae", "bd", "ea", "eab"), "a.")

str_view(fruit, "a...e")

str_view(c("a", "ab", "abb"), "ab?")
str_view(c("a", "ab", "abb"), "ab+")
str_view(c("a", "ab", "abb"), "ab*")

str_view(words, "[aeiou]x[aeiou]")
str_view(words, "[^aeiou]y[^aeiou]")

str_view(fruit, "apple|melon|nut")
str_view(fruit, "aa|ee|ii|oo|uu")

str_detect(c("a", "b", "c"), "[aeiou]")

babynames |> 
  filter(str_detect(name, "x")) |> 
  count(name, wt = n, sort = TRUE)

babynames |> 
  group_by(year) |> 
  summarize(prop_x = mean(str_detect(name, "x"))) |> 
  ggplot(aes(x = year, y = prop_x)) + 
  geom_line()

x <- c("apple", "banana", "pear")
str_count(x, "p")

str_count("abababa", "aba")
str_view("abababa", "aba")

babynames |> 
  count(name) |> 
  mutate(
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )

babynames |> 
  count(name) |> 
  mutate(
    name = str_to_lower(name),
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )

x <- c("apple", "pear", "banana")
str_replace_all(x, "[aeiou]", "-")

x <- c("apple", "pear", "banana")
str_remove_all(x, "[aeiou]")

df <- tribble(
  ~str,
  "<Sheryl>-F_34",
  "<Kisha>-F_45", 
  "<Brandon>-N_33",
  "<Sharon>-F_38", 
  "<Penny>-F_58",
  "<Justin>-M_41", 
  "<Patricia>-F_84", 
)

df |> 
  separate_wider_regex(
    str,
    patterns = c(
      "<", 
      name = "[A-Za-z]+", 
      ">-", 
      gender = ".",
      "_",
      age = "[0-9]+"
    )
  )

str_view(fruit, "^a")
str_view(fruit, "a$")
str_view(fruit, "apple")
str_view(fruit, "^apple$")

x <- c("summary(x)", "summarize(df)", "rowsum(x)", "sum(x)")
str_view(x, "sum")

str_view(x, "\\bsum\\b")

str_view("abc", c("$", "^", "\\b"))

str_replace_all("abc", c("$", "^", "\\b"), "--")

x <- "abcd ABCD 12345 -!@#%."
str_view(x, "[abc]+")
str_view(x, "[a-z]+")
str_view(x, "[^a-z0-9]+")

str_view("a-b-c", "[a-c]")

x <- "abcd ABCD 12345 -!@#%."
str_view(x, "\\d+")
str_view(x, "\\D+")
str_view(x, "\\s+")
str_view(x, "\\S+")
str_view(x, "\\w+")
str_view(x, "\\W+")


str_view(fruit, "(..)\\1")
str_view(words, "^(..).*\\1$")

sentences |> 
  str_replace("(\\w+) (\\w+) (\\w+)", "\\1 \\3 \\2") |> 
  str_view()

sentences |> 
  str_match("the (\\w+) (\\w+)") |> 
  head()

sentences |> 
  str_match("the (\\w+) (\\w+)") |> 
  as_tibble(.name_repair = "minimal") |> 
  set_names("match", "word1", "word2")

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")

x <- "Line 1\nLine 2\nLine 3"
str_view(x, ".Line")
str_view(x, regex(".Line", dotall = TRUE))

str_view(x, "^Line")
str_view(x, regex("^Line", multiline = TRUE))

phone <- regex(
  r"(
    \(?     # optional opening parens
    (\d{3}) # area code
    [)\-]?  # optional closing parens or dash
    \ ?     # optional space
    (\d{3}) # another three numbers
    [\ -]?  # optional space or dash
    (\d{4}) # four more numbers
  )", 
  comments = TRUE
)

str_extract(c("514-791-8141", "(123) 456 7890", "123456"), phone)

str_view(c("", "a", "."), fixed("."))
str_view("x X", "X")
str_view("x X", fixed("X", ignore_case = TRUE))

str_view(sentences, "^The")
str_view(sentences, "^The\\b")
str_view(sentences, "^She|He|It|They\\b")
str_view(sentences, "^(She|He|It|They)\\b")


pos <- c("He is a boy", "She had a good time")
neg <- c("Shells come from the sea", "Hadley said 'It's a great day'")

pattern <- "^(She|He|It|They)\\b"
str_detect(pos, pattern)
str_detect(neg, pattern)

str_view(words, "^[^aeiou]+$")
str_view(words[!str_detect(words, "[aeiou]")])

str_view(words, "a.*b|b.*a")
words[str_detect(words, "a") & str_detect(words, "b")]

words[
  str_detect(words, "a") &
    str_detect(words, "e") &
    str_detect(words, "i") &
    str_detect(words, "o") &
    str_detect(words, "u")
]

str_view(sentences, "\\b(red|green|blue)\\b")

rgb <- c("red", "green", "blue")
str_c("\\b(", str_flatten(rgb, "|"), ")\\b")
str_view(colors())

cols <- colors()
cols <- cols[!str_detect(cols, "\\d")]
str_view(cols)

pattern <- str_c("\\b(", str_flatten(cols, "|"), ")\\b")
str_view(sentences, pattern)

apropos("replace")
head(list.files(pattern = "\\.Rmd$"))



# Chapter 15 Exercises ----------------------------------------------------

#15.3.5
#1
babynames |>
  count(name) |>
  mutate(name = str_to_lower(name),
         vowels = str_count(name, "[aeiou]")) |>
  arrange(desc(vowels))

babynames |>
  count(name) |>
  mutate(name = str_to_lower(name),
         vowels = str_count(name, "[aeiou]"),
         name_length = str_length(name),
         vowel_prop = vowels / name_length) |>
  arrange(desc(vowel_prop))

#2
string <- "a\\b\\c\\d\\e"
str_view(string)

string <- "a//b//c//d//e"
str_view(string)

#3
string <- "Hi There"
string <- str_replace_all(string, "H", "h")
string <- str_replace_all(string, "T", "t")

#4
phone <- regex(
  r"(
    \(?     
    (\d{3}) 
    [)\-]?  
    \ ?     
    (\d{3}) 
    [\ -]? 
    (\d{4}) 
  )", 
  
)

#15.4.7
#1
string <- "'\\"
str_detect(string, fixed("'\\"))
string <- "$^$"
str_detect(string, fixed("$^$"))

#3
str_view(words, "^y")
str_view(words, "^[^y]")
str_view(words, "x$")
str_view(words, "^...$")
str_view(words, "^.{7,}$")
str_view(words, "^([aeiou][^aeiou])$+")

#4
british_words <- "a(er|ir)(o|)plane|alumin(i|)um analogu?e|a(r|)se|cent(re|er)|defen[cs]e|dou(gh)?nut|gr[ae]y|modell?ing|s[ck]eptic|summari[sz]e"

#5

#could not figure this one out.
str_replace_all(words, "^.", "(\\w)$")

#15.6.4
#1
str_view(words, "^x|x$")
str_detect(words, "^x")
str_detect(words, "x$")

str_view(words,"^[aeiou].*[^aeiou]$")
str_detect(words, "^[aeiou]")
str_detect(words, "[^aeiou]$")

str_subset(words, "(?=.*a)(?=.*e)(?=.*i)(?=.*o)(?=.*u)")

#2
cei_words <- str_subset(words, "cei")
cie_words <- str_subset(words, "cie")

str_subset(words, "(?<!c)ei")

#3
all_colors <- colors()
colors_df <- tibble(color = all_colors)
colors_df <- colors_df |>
  mutate(
    modifier = str_extract(color, "^[a-z]+(?=[A-Z])")  # lowercase letters before capital
  )

#4
all_data <- data(package = "datasets")$results[, "Item"]
View(all_data)
all_data <- clean_names <- str_remove(all_data, " \\(.*\\)")
patterned_datasets <- str_c("\\b(", str_c(all_data, collapse = "|"), ")\\b")
