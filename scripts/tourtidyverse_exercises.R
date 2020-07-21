# Code to simulate the data set used in the exercises
uni_results <- data.frame(student_id = 1:100,
                          gender = sample(c("MALE", "FEMALE"), 100, replace = TRUE,
                                          prob = c(0.5, 0.5)),
                          test_a_score = round(runif(100, min = 30, max = 92), 0),
                          test_b_score = round(runif(100, min = 25, max = 84), 0),
                          IMC = sample(c("YES", "no"), 100, replace = TRUE,
                                       prob = c(0.05, 0.96)),
                          course = sample(c("Biology", "Mathematics", "Physics"),
                                          100, replace = TRUE),
                          graduation_date = sample(c("2019-7-12", "2019-9-5"),
                                                   100, replace = TRUE)
                          )

write_csv(uni_results, "uni_results.csv")

# Exercise 1: Read in the uni_results.csv

uni_results <- read_csv("uni_results.csv")


# Exercise 2: tidy uni_results

uni_results <- gather(uni_results, key = "test", 
                      value = "test_score", 
                      test_a_score, test_b_score)

# Exercise 3: dplyr verbs
#1.
filter(uni_results, gender == "FEMALE")

#2.
arrange(uni_results, desc(test_score))

#3.
uni_results %>%
group_by(course, test) %>%
  summarise(mean(test_score))

# Exercise 4: 
uni_results <- mutate(uni_results,
  test = factor(test),
  test = fct_recode(test, A = "test_a_score", B = "test_b_score")
)

class(uni_results$graduation_date)

uni_results <- mutate(uni_results,
  graduation_date = ymd(graduation_date)
)

# Exercise 5: gradually build up plot
ggplot(data = uni_results,
       mapping = aes(x = test, y = test_score, colour = course)) +
  geom_boxplot() +
  labs(title = "Box plot of test score by test",
       x = "Test",
       y = "Test Score") +
  scale_color_discrete("Course")


# Exercise 6: purrr

map(mtcars, mean)

uni_nested <- uni_results %>%
  group_by(course) %>% 
  nest()

map(uni_nested$data, ~var(.$test_score)) %>% setNames(uni_nested$course)
