IF OBJECT_ID('tempdb..#movies_actors') IS NOT NULL DROP TABLE #movies_actors
IF OBJECT_ID('tempdb..#movies_languages') IS NOT NULL DROP TABLE #movies_languages
IF OBJECT_ID('tempdb..#movies_dates') IS NOT NULL DROP TABLE #movies_dates
IF OBJECT_ID('tempdb..#blockbusters') IS NOT NULL DROP TABLE #blockbusters
IF OBJECT_ID('tempdb..#actors_revenue_rates') IS NOT NULL DROP TABLE #actors_revenue_rates
IF OBJECT_ID('tempdb..#directors_revenue_rates') IS NOT NULL DROP TABLE #directors_revenue_rates
IF OBJECT_ID('tempdb..#producers_revenue_rates') IS NOT NULL DROP TABLE #producers_revenue_rates
IF OBJECT_ID('tempdb..#movies_runtime') IS NOT NULL DROP TABLE #movies_runtime
IF OBJECT_ID('tempdb..#movies_crew') IS NOT NULL DROP TABLE #movies_crew
IF OBJECT_ID('tempdb..#movies_genres') IS NOT NULL DROP TABLE #movies_genres
IF OBJECT_ID('tempdb..#movies_actors_revenues') IS NOT NULL DROP TABLE #movies_actors_revenues
IF OBJECT_ID('tempdb..#movies_directors_revenues') IS NOT NULL DROP TABLE #movies_directors_revenues
IF OBJECT_ID('tempdb..#movies_producers_revenues') IS NOT NULL DROP TABLE #movies_producers_revenues


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


select 
m.movie_id,
MAX(arr.avg_revenue) max_actor_avg_revenue,  -- max avg revenue between all movie actors
ROUND(AVG(arr.avg_revenue), 1) all_actors_avg_revenue,  -- avg revenue for all movie actors
MAX(arr.blockbusters_participations) max_actor_blockbusters_participations,
ROUND(AVG(arr.avg_revenue), 1) all_actors_avg_blockbusters_participations
into #movies_actors_revenues
from [dbo].[movies] m inner join 
     [dbo].[movie_cast] mc on m.movie_id = m_movie_id inner join
     #actors_revenue_rates arr on arr.actor_id = mc.actor_id
group by m.movie_id


select 
m.movie_id,
MAX(drr.avg_revenue) max_director_avg_revenue,  -- max avg revenue between all movie directors
ROUND(AVG(drr.avg_revenue), 1) all_directors_avg_revenue,  -- avg revenue for all movie directors
MAX(drr.blockbusters_participations) max_director_blockbusters_participations,
ROUND(AVG(drr.avg_revenue), 1) all_directors_avg_blockbusters_participations
into #movies_directors_revenues
from [dbo].[movies] m inner join 
     [dbo].[movie_crew] mc on m.movie_id = m_movie_id and mc.job = 'Director' inner join
     #directors_revenue_rates drr on drr.crew_id = mc.crew_id
group by m.movie_id


select 
m.movie_id,
MAX(prr.avg_revenue) max_producer_avg_revenue,  -- max avg revenue between all movie producers
ROUND(AVG(prr.avg_revenue), 1) all_producers_avg_revenue,  -- avg revenue for all movie producers
MAX(prr.blockbusters_participations) max_producer_blockbusters_participations,
ROUND(AVG(prr.avg_revenue), 1) all_producers_avg_blockbusters_participations
into #movies_producers_revenues
from [dbo].[movies] m inner join 
     [dbo].[movie_crew] mc on m.movie_id = m_movie_id and mc.job = 'Producer' inner join
     #producers_revenue_rates prr on prr.crew_id = mc.crew_id
group by m.movie_id


select movie_id, runtime,
round(runtime/10, 0) runtime_intervals,
CASE WHEN runtime < 90 THEN 0 ELSE (CASE WHEN runtime < 120 THEN 1 ELSE 2 END) END runtime_cat
into #movies_runtime
from [dbo].[movies]


select movie_id, count(*) crew_total_count,
SUM(case when mc.[department] = 'writing' THEN 1 ELSE 0 END) crew_writing_count,
SUM(case when mc.[department] = 'directing' THEN 1 ELSE 0 END) crew_directing_count,
SUM(case when mc.[department] = 'art' THEN 1 ELSE 0 END) crew_art_count,
SUM(case when mc.[department] = 'sound' THEN 1 ELSE 0 END) crew_sound_count,
SUM(case when mc.[department] = 'costume&makeup' THEN 1 ELSE 0 END) crew_costume_makeup_count,
SUM(case when mc.[department] = 'visual_effects' THEN 1 ELSE 0 END) crew_visual_effects_count,
SUM(case when mc.[department] = 'production' THEN 1 ELSE 0 END) crew_production_count,
SUM(case when mc.[department] = 'actors' THEN 1 ELSE 0 END) crew_actors_count,
SUM(case when mc.[department] = 'camera' THEN 1 ELSE 0 END) crew_camera_count,
SUM(case when mc.[department] = 'lighting' THEN 1 ELSE 0 END) crew_lighting_count,
SUM(case when mc.[department] = 'editing' THEN 1 ELSE 0 END) crew_editing_count
into #movies_crew
from [dbo].[movies] m inner join 
     [dbo].[movie_crew] mc on mc.movie_id = m.movie_id inner join 
	 [dbo].[crew_dim] c on c.crew_id = mc.crew_id
group by m.movie_id


select movie_id
SUM(case when g.[name] = 'Action' THEN 1 ELSE 0 END) sw_genre_action,
SUM(case when g.[name] = 'Adventure' THEN 1 ELSE 0 END) sw_genre_adventure,
SUM(case when g.[name] = 'Animation' THEN 1 ELSE 0 END) sw_genre_animation,
SUM(case when g.[name] = 'Comedy' THEN 1 ELSE 0 END) sw_genre_comedy,
SUM(case when g.[name] = 'Crime' THEN 1 ELSE 0 END) sw_genre_crime,
SUM(case when g.[name] = 'Documentary' THEN 1 ELSE 0 END) sw_genre_documentary,
SUM(case when g.[name] = 'Drama' THEN 1 ELSE 0 END) sw_genre_drama,
SUM(case when g.[name] = 'Family' THEN 1 ELSE 0 END) sw_genre_family,
SUM(case when g.[name] = 'Fantasy' THEN 1 ELSE 0 END) sw_genre_fantasy,
SUM(case when g.[name] = 'Foreign' THEN 1 ELSE 0 END) sw_genre_foreign,
SUM(case when g.[name] = 'History' THEN 1 ELSE 0 END) sw_genre_history,
SUM(case when g.[name] = 'Horror' THEN 1 ELSE 0 END) sw_genre_horror,
SUM(case when g.[name] = 'Music' THEN 1 ELSE 0 END) sw_genre_music,
SUM(case when g.[name] = 'Mystery' THEN 1 ELSE 0 END) sw_genre_mystery,
SUM(case when g.[name] = 'Romance' THEN 1 ELSE 0 END) sw_genre_romance,
SUM(case when g.[name] = 'Science Fiction' THEN 1 ELSE 0 END) sw_genre_science_fiction,
SUM(case when g.[name] = 'Thriller' THEN 1 ELSE 0 END) sw_genre_thriller,
SUM(case when g.[name] = 'TV Movie' THEN 1 ELSE 0 END) sw_genre_tv_movie,
SUM(case when g.[name] = 'War' THEN 1 ELSE 0 END) sw_genre_war,
SUM(case when g.[name] = 'Western' THEN 1 ELSE 0 END) sw_genre_western,
count(*) genres_total

into #movies_genres
from [dbo].[movies] m inner join 
     [dbo].[movies_genres] mg on mg.movie_id = m.movie_id inner join 
	 [dbo].[genres_dim] g on g.genre_id = mg.genre_id
group by m.movie_id



select 
    ma.movie_id,
	ma.original_title,
    ma.revenue,
	ma.budget,
	ma.popularity,
	ma.main_actor_id,
	ma.main_actor_name,
    ml.original_language,
	ml.language_en,
	ml.language_fr,
	ml.language_es,
	ml.language_de,
	ml.language_ru,
	ml.language_it,
	ml.language_ja,
	md.release_date,
	md.release_year,
	md.release_month,
	md.release_week,
	md.release_weekday,
	mr.runtime,
    mr.runtime_intervals,
    mr.runtime_cat,
	mc.crew_total_count,
	mc.crew_writing_count,
	mc.crew_directing_count,
	mc.crew_art_count,
	mc.crew_sound_count,
	mc.crew_costume_makeup_count,
	mc.crew_visual_effects_count,
	mc.crew_production_count,
	mc.crew_actors_count,
	mc.crew_camera_count,
	mc.crew_lighting_count,
	mc.crew_editing_count,
	mg.sw_genre_action,
	mg.sw_genre_adventure,
	mg.sw_genre_animation,
	mg.sw_genre_comedy,
	mg.sw_genre_crime,
	mg.sw_genre_documentary,
	mg.sw_genre_drama,
	mg.sw_genre_family,
	mg.sw_genre_fantasy,
	mg.sw_genre_foreign,
	mg.sw_genre_history,
	mg.sw_genre_horror,
	mg.sw_genre_music,
	mg.sw_genre_mystery,
	mg.sw_genre_romance,
	mg.sw_genre_science_fiction,
	mg.sw_genre_thriller,
	mg.sw_genre_tv_movie,
	mg.sw_genre_war,
	mg.sw_genre_western,
	mg.genres_total,
	mar.max_actor_avg_revenue,  -- max avg revenue between all movie actors
	mar.all_actors_avg_revenue,  -- avg revenue for all movie actors
	mar.max_actor_blockbusters_participations,
	mar.all_actors_avg_blockbusters_participations,
	mdr.max_director_avg_revenue,  -- max avg revenue between all movie directors
	mdr.all_directors_avg_revenue,  -- avg revenue for all movie directors
	mdr.max_director_blockbusters_participations,
	mdr.all_directors_avg_blockbusters_participations,
	mpr.max_producer_avg_revenue,  -- max avg revenue between all movie producers
	mpr.all_producers_avg_revenue,  -- avg revenue for all movie producers
	mpr.max_producer_blockbusters_participations,
	mpr.all_producers_avg_blockbusters_participations
from #movies_actors ma left outer join
     #movies_languages ml on ml.movie_id = ma.movie_id left outer join
	 #movies_dates md on md.movie_id = ma.movie_id left outer join
	 #movies_runtime mr on mr.movie_id = ma.movie_id left outer join
	 #movies_crew mc on mc.movie_id = ma.movie_id left outer join
	 #movies_genres mg on mg.movie_id = ma.movie_id left outer join
	 #movies_actors_revenues mar on mar.movie_id = ma.movie_id left outer join
	 #movies_directors_revenues mdr on mdr.movie_id = ma.movie_id left outer join
	 #movies_producers_revenues mpr on mpr.movie_id = ma.movie_id 



IF OBJECT_ID('tempdb..#movies_actors') IS NOT NULL DROP TABLE #movies_actors
IF OBJECT_ID('tempdb..#movies_languages') IS NOT NULL DROP TABLE #movies_languages
IF OBJECT_ID('tempdb..#movies_dates') IS NOT NULL DROP TABLE #movies_dates
IF OBJECT_ID('tempdb..#blockbusters') IS NOT NULL DROP TABLE #blockbusters
IF OBJECT_ID('tempdb..#actors_revenue_rates') IS NOT NULL DROP TABLE #actors_revenue_rates
IF OBJECT_ID('tempdb..#directors_revenue_rates') IS NOT NULL DROP TABLE #directors_revenue_rates
IF OBJECT_ID('tempdb..#producers_revenue_rates') IS NOT NULL DROP TABLE #producers_revenue_rates
IF OBJECT_ID('tempdb..#movies_runtime') IS NOT NULL DROP TABLE #movies_runtime
IF OBJECT_ID('tempdb..#movies_crew') IS NOT NULL DROP TABLE #movies_crew
IF OBJECT_ID('tempdb..#movies_genres') IS NOT NULL DROP TABLE #movies_genres
IF OBJECT_ID('tempdb..#movies_actors_revenues') IS NOT NULL DROP TABLE #movies_actors_revenues
IF OBJECT_ID('tempdb..#movies_directors_revenues') IS NOT NULL DROP TABLE #movies_directors_revenues
IF OBJECT_ID('tempdb..#movies_producers_revenues') IS NOT NULL DROP TABLE #movies_producers_revenues