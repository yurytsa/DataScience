--  a.   Num of students per Department

select r.DepartmentName, count(*) NumOfStudents
from(
	select distinct d.DepartmentName, s.StudentId
	from [dbo].[Departments] d inner join 
		 [dbo].[Courses] c on c.DepartmentID = d.DepartmentId inner join
		 [dbo].[Classrooms] cr on cr.CourseId = c.CourseId inner join 
		 [dbo].[Students] s on s.StudentId = cr.StudentId
) r
group by r.DepartmentName 



--  b. Number of students per english course + all students 

select c.CourseName, Sum(1) Students
from [dbo].[Teachers] t inner join 
	[dbo].[Courses] c on c.TeacherId = t.TeacherId inner join
	[dbo].[Departments] d on d.DepartmentId = c.DepartmentID and d.DepartmentName = 'English' inner join 
	[dbo].[Classrooms] cr on cr.CourseId = c.CourseId inner join 
	[dbo].[Students] s on s.StudentId = cr.StudentId
group by c.CourseName 
 
select count(*) AllStudents
from(
	select distinct d.DepartmentName, s.StudentId
	from [dbo].[Departments] d inner join 
		 [dbo].[Courses] c on c.DepartmentID = d.DepartmentId and d.DepartmentName = 'English' inner join
		 [dbo].[Classrooms] cr on cr.CourseId = c.CourseId inner join 
		 [dbo].[Students] s on s.StudentId = cr.StudentId
) r




--  c. Science department - num of big and small classes
with cte_ClassesInfo as
(
	select d.DepartmentName, c.CourseName, Count(cr.StudentId) StudentsInClassroom
	from [dbo].[Courses] c inner join
		 [dbo].[Departments] d on d.DepartmentId = c.DepartmentID and d.DepartmentName = 'Science' inner join 
		 [dbo].[Classrooms] cr on cr.CourseId = c.CourseId 
	group by d.DepartmentName, c.CourseName
)
select 
DepartmentName, 
Sum(CASE when StudentsInClassroom >= 22 then 1 else 0 end) as BigRooms,
Sum(CASE when StudentsInClassroom < 22 then 1 else 0 end) as SmallRooms
from cte_ClassesInfo
group by DepartmentName




--  d. Number of male/femail students (she is wrong :))
select 
Sum(CASE Gender WHEN 'F' THEN 1 ELSE 0 END) FemaleStudents,
Sum(CASE Gender WHEN 'F' THEN 0 ELSE 1 END) MaleStudents
from [dbo].[Students]




--  e. Percentage of male/female students per course (>70%)
select c.CourseName,
	Sum(CASE Gender WHEN 'F' THEN 1 ELSE 0 END) * 100 / Sum(1) FemaleStudents,
	Sum(CASE Gender WHEN 'F' THEN 0 ELSE 1 END) * 100 / Sum(1) MaleStudents
from [dbo].[Courses] c inner join
		[dbo].[Classrooms] cr on cr.CourseId = c.CourseId inner join
		[dbo].[Students] s on s.StudentId = cr.StudentId
group by c.CourseName
having Sum(CASE Gender WHEN 'F' THEN 1 ELSE 0 END) * 100 / Sum(1) > 70 or 
	    Sum(CASE Gender WHEN 'F' THEN 0 ELSE 1 END) * 100 / Sum(1) > 70




-- g-f. Num of students with degree upper to 80 and below 60 per Department 
with cte_StudentDegreesInfo as 
(
	select d.DepartmentName, s.StudentId, 
	(CASE WHEN Sum(CASE when cr.degree > 80 then 1 else 0 end) > 0 then 1 else 0 end)  GraduatedCoursesUpper80,
	(CASE WHEN Sum(CASE when cr.degree < 60 then 1 else 0 end) > 0 then 1 else 0 end)  GraduatedCoursesBelow60
	from
		[dbo].[Courses] c inner join
		[dbo].[Departments] d on d.DepartmentId = c.DepartmentID inner join 
		[dbo].[Classrooms] cr on cr.CourseId = c.CourseId inner join 
		[dbo].[Students] s on s.StudentId = cr.StudentId
    group by d.DepartmentName, s.StudentId
)

select DepartmentName, 
Count(*) AllStudents,
Sum(GraduatedCoursesUpper80) GraduatedUpper80, 
Sum(GraduatedCoursesUpper80) * 100 / Count(*) GraduatedUpper80Percentage,
Sum(GraduatedCoursesBelow60) GraduatedBelow60, 
Sum(GraduatedCoursesBelow60) * 100 / Count(*) GraduatedBelow60Percentage
from cte_StudentDegreesInfo
group by DepartmentName


--   h. Sort teachers by mean degrees of their students

select t.FirstName + ' ' + t.LastName TeacherName, Avg(cr.degree) AverageDegree
from [dbo].[Teachers] t inner join 
	 [dbo].[Courses] c on c.TeacherId = t.TeacherId inner join
     [dbo].[Classrooms] cr on cr.CourseId = c.CourseId 
group by t.FirstName + ' ' + t.LastName
order by AverageDegree desc



--  VIEW : a. -- Courses per department with teacher name and num of students

--CREATE VIEW [dbo].[vw_CoursesInfo]
--AS
--select d.DepartmentName, c.CourseName, t.FirstName + ' ' + t.LastName TeacherName, SUM(1) NumOfStudents
--from	[dbo].[Courses] c inner join
--		[dbo].[Departments] d on d.DepartmentId = c.DepartmentID inner join 
--		[dbo].[Classrooms] cr on cr.CourseId = c.CourseId inner join 
--		[dbo].[Students] s on s.StudentId = cr.StudentId inner join
--		[dbo].[Teachers] t on t.TeacherId = c.TeacherId
--group by d.DepartmentName, c.CourseName, t.FirstName, t.LastName
--GO

select * from [dbo].[vw_CoursesInfo]
order by 1,2,3



--  VIEW : b. -- Courses per department with teacher name and num of students

--CREATE VIEW [dbo].[vw_StudentsInfo]
--AS
--select piv.StudentName, 
--StudentCourses.NumOfCourses,
--CEILING(piv.Arts) ArtsAvg, CEILING(piv.English) EnglishAvg, CEILING(piv.Science) ScienceAvg, CEILING(piv.Sport) SportAvg from
--(
-- select s.FirstName + ' ' + s.LastName StudentName,  
-- d.DepartmentName, 
-- AVG(cr.degree) AverageDegree
-- from [dbo].[Students] s inner join 
--      [dbo].[Classrooms] cr on cr.StudentId = s.StudentId inner join
--      [dbo].[Courses] c on c.CourseId = cr.CourseId inner join 
--      [dbo].[Departments] d on d.DepartmentId = c.DepartmentID 
-- GROUP BY s.FirstName + ' ' + s.LastName, d.DepartmentName
-- ) a
--PIVOT (
--	AVG(a.AverageDegree) 
--	FOR a.DepartmentName in (Arts, English,Science,Sport)
--) piv
--left outer join
--(
--	select s.FirstName + ' ' + s.LastName StudentName, count(cr.CourseId) NumOfCourses
--	from  [dbo].[Classrooms] cr inner join [dbo].[Courses] c on c.CourseId = cr.CourseId
--	                            inner join [dbo].[Students] s on s.StudentId = cr.StudentId
--	group by s.FirstName + ' ' + s.LastName 
--) StudentCourses on StudentCourses.StudentName = piv.StudentName
--GO

SELECT * FROM [College].[dbo].[vw_StudentsInfo]

