if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load_gh("trinker/wakefield")

set.seed(1)
random_student_data <- r_data_frame(
    n = 500,
    id(name = "studentID"),
    sex(name = "sex"),
    race(x = c("White", "Hispanic", "Black", "Asian", "Native"), prob = c(0.64, 0.16, 0.12, 0.06, 0.02), name = "race"),
    age(x = 13:15, name = "age"),
    grade_level(x = "9", name = "grade_level"),
    likert_5(x = c("1", "2", "3", "4", "5"), name = "math_motivation"),
    likert_7(x = c("1", "2", "3", "4", "5", "6", "7"), name = "perceived_math_meaning_week1"),
    likert_7(x = c("1", "2", "3", "4", "5", "6", "7"), name = "perceived_math_meaning_week2")
)

write.csv(random_student_data, file = "data-raw/random_student_data.csv")
