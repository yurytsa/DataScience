IF OBJECT_ID('tempdb..#movies_actors') IS NOT NULL DROP TABLE #movies_actors
IF OBJECT_ID('tempdb..#movies_languages') IS NOT NULL DROP TABLE #movies_languages
IF OBJECT_ID('tempdb..#movies_dates') IS NOT NULL DROP TABLE #movies_dates
IF OBJECT_ID('tempdb..#blockbusters') IS NOT NULL DROP TABLE #blockbusters
IF OBJECT_ID('tempdb..#actors_revenue_rates') IS NOT NULL DROP TABLE #actors_revenue_rates
IF OBJECT_ID('tempdb..#directors_revenue_rates') IS NOT NULL DROP TABLE #directors_revenue_rates
IF OBJECT_ID('tempdb..#producers_revenue_rates') IS NOT NULL DROP TABLE #producers_revenue_rates
IF OBJECT_ID('tempdb..#movies_runtime') IS NOT NULL DROP TABLE #movies_runtime



select 
    m.movie_id,
	m.original_title,
	m.revenue,
	m.budget,
	m.popularity,
	m.release_date,
	m.runtime,
	count(a.actor_id) actors_count,
	min(mainactors.actor_id) main_actor_id,
	min(a1.name) main_actor_name
into #movies_actors
from [dbo].[movies] m left outer join 
		 [dbo].[movie_cast] mc on mc.movie_id = m.movie_id left outer join
		 [dbo].[actors_dim] a on a.actor_id = mc.actor_id left outer join
		 (select v.actor_id from [dbo].[movie_cast] v where v.[order] = 0) mainactors on mainactors.actor_id = a.actor_id  left outer join
		 [dbo].[actors_dim] a1 on a1.actor_id = mainactors.actor_id

	group by m.movie_id,
	m.original_title,
	m.revenue,
	m.budget,
	m.popularity,
	m.release_date,
	m.runtime,
	m.original_language



select 
m.movie_id, 
m.original_language,
CASE WHEN m.original_language = 'en' THEN 1 ELSE 0 END language_en,
CASE WHEN m.original_language = 'fr' THEN 1 ELSE 0 END language_fr,
CASE WHEN m.original_language = 'es' THEN 1 ELSE 0 END language_es,
CASE WHEN m.original_language = 'de' THEN 1 ELSE 0 END language_de,
CASE WHEN m.original_language = 'ru' THEN 1 ELSE 0 END language_ru,
CASE WHEN m.original_language = 'it' THEN 1 ELSE 0 END language_it,
CASE WHEN m.original_language = 'ja' THEN 1 ELSE 0 END language_ja
into #movies_languages
from [dbo].[movies] m 


select 
m.movie_id, 
m.release_date,
Year(m.release_date) release_year,
Month(m.release_date) release_month,
DATEPART(ww,m.release_date) release_week,
DATEPART(WEEKDAY,m.release_date) release_weekday
into #movies_dates
from [dbo].[movies] m 

SELECT top 10 percent movie_id
into #blockbusters
FROM [dbo].[movies]
order by revenue desc

-- temp table that aggregating information about actors: number of blockbusters (top 10% by revenue) actor participated in,
-- average revenue of all movies actor participated in, total number of movies
select a.actor_id, a.name
,SUM(CASE WHEN b.movie_id is null THEN 0 ELSE 1 END) blockbusters_participations
,ROUND(AVG(CAST(m.revenue as float)/CAST(1000000 as float)),1) avg_revenue
,count(m.movie_id) out_of
into #actors_revenue_rates
from [dbo].[actors_dim] a inner join 
		[dbo].[movie_cast] mc on mc.actor_id = a.actor_id inner join 
		[dbo].[movies] m on m.movie_id = mc.movie_id left outer join
		#blockbusters b on b.movie_id = mc.movie_id
group by a.actor_id, a.name

-- temp table that aggregating information about directors: number of blockbusters (top 10% by revenue) director participated in,
-- average revenue of all movies director participated in, total number of movies
select c.crew_id, c.name
,SUM(CASE WHEN b.movie_id is null THEN 0 ELSE 1 END) blockbusters_participation
,ROUND(AVG(CAST(m.revenue as float)/CAST(1000000 as float)),1) avg_revenue
,count(m.movie_id) out_of
into #directors_revenue_rates
from [dbo].[crew_dim] c inner join 
		[dbo].[movie_crew] mc on mc.crew_id = c.crew_id and mc.job = 'Director' inner join 
		[dbo].[movies] m on m.movie_id = mc.movie_id left outer join
		#blockbusters b on b.movie_id = mc.movie_id
group by c.crew_id, c.name

-- temp table that aggregating information about directors: number of blockbusters (top 10% by revenue) producer participated in,
-- average revenue of all movies producer participated in, total number of movies
select c.crew_id, c.name
,SUM(CASE WHEN b.movie_id is null THEN 0 ELSE 1 END) blockbusters_participation
,ROUND(AVG(CAST(m.revenue as float)/CAST(1000000 as float)),1) avg_revenue
,count(m.movie_id) out_of
into #producers_revenue_rates
from [dbo].[crew_dim] c inner join 
		[dbo].[movie_crew] mc on mc.crew_id = c.crew_id and mc.job = 'Producer' inner join 
		[dbo].[movies] m on m.movie_id = mc.movie_id left outer join
		#blockbusters b on b.movie_id = mc.movie_id
group by c.crew_id, c.name


select movie_id, runtime,
round(runtime/10, 0) runtime_intervals,
CASE WHEN runtime < 90 THEN 0 ELSE (CASE WHEN runtime < 120 THEN 1 ELSE 2 END) END runtime_cat
into #movies_runtime
from [dbo].[movies]








