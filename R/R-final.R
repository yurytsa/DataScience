###R Intro - Final Exercise


library(DBI)
library(dplyr)

### In windows, Using a ODBC DNS (predefined connection name)
### Some possible strings for the driver:
### the DSN must be the same as you created in the ODBC (check it!)
#driver <- "Driver={SQL Server};DSN=College;Trusted_Connection=yes;"

driver <- "Driver={SQL Server Native Connection 11.0};DSN=College;Trusted_Connection=yes;"

### XXXXX\\XXXXX is the name of the server as it appears in the SQL server management studio
### COLLEGE is the name of the database (check how do you called it in your local server)
#driver <- "Driver={SQL Server Native Connection 11.0};Server=YURYT-LT\\DS11;Database=College;Trusted_Connection=True;"


### Try with the diferent driver strings to see what works for you
conn <- dbConnect(odbc::odbc(), connection_string = driver)


### Get the students table
students = dbGetQuery(conn, "SELECT * FROM Students;") 
classrooms = dbGetQuery(conn, "SELECT * FROM Classrooms;") 
teachers = dbGetQuery(conn, "SELECT * FROM Teachers;") 
courses = dbGetQuery(conn, "SELECT * FROM Courses;") 
departments = dbGetQuery(conn, "SELECT * FROM Departments;")

stud_class <- dplyr::inner_join(students, classrooms, by="StudentId")
stud_class_course <- dplyr::inner_join(stud_class, courses, by="CourseId")
stud_class_course_dep <- dplyr::inner_join(stud_class_course, departments, by=c("DepartmentID" = "DepartmentId"))


 
###Questions
#Q1. Count the number of students on each department

res1 <- stud_class_course_dep %>%
  distinct(StudentId, DepartmentName, .keep_all = TRUE) %>%
  group_by(DepartmentName) %>%
  summarise(num_students = n(),)

res1


#Q2. How many students have each course of the English department and the total number of students in the department?

res2_1 = stud_class_course_dep %>% 
  filter(DepartmentName == 'English') %>%
  group_by(CourseName) %>% 
  summarise(num_students = n(),)
res2_1

res2_2 <- stud_class_course_dep %>% 
  filter(DepartmentName == 'English') %>%
  distinct(StudentId, .keep_all = TRUE) %>%
  group_by() %>% 
  summarise(num_students = n(),)
res2_2  


#Q3. How many small (<22 students) and large (22+ students) classrooms are needed for the Science department?

res3 = stud_class_course_dep %>% 
  filter(DepartmentName == 'Science') %>%
  group_by(CourseId) %>% 
  summarise(num_students = n(),) %>% 
  mutate(classroom_size = ifelse(num_students < 22,"Small classrooms",'Big classrooms'))  %>% 
  group_by(classroom_size) %>% 
  summarise(num_classrooms = n(),)
res3


#Q4. A feminist student claims that there are more male than female in the College. Justify if the argument is correct


#Q5. For which courses the percentage of male/female students is over 70%?


#Q6. For each department, how many students passed with a grades over 80?


#Q7. For each department, how many students passed with a grades under 60?


#Q8. Rate the teachers by their average student's grades (in descending order).


#Q9. Create a dataframe showing the courses, departments they are associated with, the teacher in each course, and the number of students enrolled in the course (for each course, department and teacher show the names).


#Q10. Create a dataframe showing the students, the number of courses they take, the average of the grades per class, and their overall average (for each student show the student name).



