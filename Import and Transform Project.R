
# Import and Transform Project --------------------------------------------

employees <- read_csv("Data for Importation.csv")

employees <- employees |>
  janitor::clean_names()

#1
employees <- employees |>
  separate(employee_name, into = c("Last", "First"), sep = ",")

employees <- employees |>
  relocate(First)
#2
employees <- employees |>
  mutate(First = as.character(First),
         Last = as.character(Last),
         building = as.character(building),
         department = as.character(department),
         status = as.character(status),
         hire_date = as.double(hire_date),
         benefits = as.character(benefits),
         compensation = as.double(compensation),
         job_rating = as.double(job_rating),
         new_comp = as.double(new_comp))

employees <- employees |>
  mutate(buliding = factor(building),
         department = factor(department),
         status = factor(status),
         )

#3
employees <- employees |>
  mutate(hire_date = as.Date(hire_date, origin = "1899-12-30"))

#4
employees <- employees |>
  mutate(tenure = (year(today()) - year(hire_date)))

#5
employees <- employees |>
  mutate(formatted_compensation = label_dollar()(compensation))

#6
employees <- employees |>
  mutate(formatted_new_comp = label_dollar()(new_comp))

#7 |>
employees <- employees |>
  mutate(compensation_percentage_increase = ((new_comp - compensation) / compensation),
  formatted_percentage_increase = label_percent()(compensation_percentage_increase))

#8
employees |>
  group_by(department, tenure) |>
  summarize(avg_increase = mean(compensation_percentage_increase, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = tenure, y = avg_increase, color = department)) +
  geom_line()
