USE BoxOffice

IF OBJECT_ID('tempdb..#movies_info') IS NOT NULL DROP TABLE #movies_info
IF OBJECT_ID('tempdb..#movies_actors') IS NOT NULL DROP TABLE #movies_actors
IF OBJECT_ID('tempdb..#blockbusters') IS NOT NULL DROP TABLE #blockbusters
IF OBJECT_ID('tempdb..#actors_revenue_rates') IS NOT NULL DROP TABLE #actors_revenue_rates
IF OBJECT_ID('tempdb..#directors_revenue_rates') IS NOT NULL DROP TABLE #directors_revenue_rates
IF OBJECT_ID('tempdb..#producers_revenue_rates') IS NOT NULL DROP TABLE #producers_revenue_rates
IF OBJECT_ID('tempdb..#movies_crew') IS NOT NULL DROP TABLE #movies_crew
IF OBJECT_ID('tempdb..#movies_genres') IS NOT NULL DROP TABLE #movies_genres
IF OBJECT_ID('tempdb..#movies_actors_revenues') IS NOT NULL DROP TABLE #movies_actors_revenues
IF OBJECT_ID('tempdb..#movies_directors_revenues') IS NOT NULL DROP TABLE #movies_directors_revenues
IF OBJECT_ID('tempdb..#movies_producers_revenues') IS NOT NULL DROP TABLE #movies_producers_revenues
IF OBJECT_ID('tempdb..#mainactors') IS NOT NULL DROP TABLE #mainactors
IF OBJECT_ID('tempdb..#movies_keywords') IS NOT NULL DROP TABLE #movies_keywords
 
---- correct release date
--UPDATE movies 
--SET release_date = CASE WHEN (YEAR(release_date) > 2017) THEN DATEADD(YEAR,-100,release_date) ELSE (release_date) END

select 
    -- info
	m.movie_id,
	m.original_title,
	m.revenue,
	m.budget,
	m.popularity,
	-- languages
	m.original_language,
	CASE WHEN m.original_language = 'en' THEN 1 ELSE 0 END language_en,
	CASE WHEN m.original_language = 'fr' THEN 1 ELSE 0 END language_fr,
	CASE WHEN m.original_language = 'es' THEN 1 ELSE 0 END language_es,
	CASE WHEN m.original_language = 'de' THEN 1 ELSE 0 END language_de,
	CASE WHEN m.original_language = 'ru' THEN 1 ELSE 0 END language_ru,
	CASE WHEN m.original_language = 'it' THEN 1 ELSE 0 END language_it,
	CASE WHEN m.original_language = 'ja' THEN 1 ELSE 0 END language_ja,
	-- dates
	m.release_date,
	Year(m.release_date) release_year,
	Month(m.release_date) release_month,
	DATEPART(ww,m.release_date) release_week,
	DATEPART(WEEKDAY,m.release_date) release_weekday,
	-- runtime
	m.runtime,
	round(m.runtime/10, 0) runtime_intervals,
	CASE WHEN m.runtime < 90 THEN 'Short' ELSE (CASE WHEN m.runtime < 120 THEN 'Intermediate' ELSE 'Long' END) END runtime_cat
into #movies_info
from [dbo].[movies] m 

select m.movie_id,
       CASE WHEN count(mc.collection_id) > 0 THEN 1 ELSE 0 END is_collection,
	   count(mk.keyword_id) keywords_cnt
into #movies_keywords
from [dbo].[movies] m left outer join
     [dbo].[movie_collection] mc on mc.movie_id = m.movie_id left outer join 
     [dbo].[movie_keywords] mk on mk.movie_id = m.movie_id
group by m.movie_id     


-- temp table get top 3 actors for every movie and total actors
select m.movie_id,
max(CASE WHEN mc.[order] = 0 THEN mc.actor_id ELSE null END) actor0_id,
max(CASE WHEN mc.[order] = 1 THEN mc.actor_id ELSE null END) actor1_id,
max(CASE WHEN mc.[order] = 2 THEN mc.actor_id ELSE null END) actor2_id
into #mainactors
from [dbo].[movie_cast] mc inner join 
     [dbo].[movies] m on m.movie_id = mc.movie_id inner join 
     [dbo].[actors_dim] a on a.actor_id = mc.actor_id
group by m.movie_id
    
-- temp table contains aggregated info about actors in movie
select 
    m.movie_id,
	count(a.actor_id) actors_cnt

into #movies_actors
from [dbo].[movies] m left outer join 
		 [dbo].[movie_cast] mc on mc.movie_id = m.movie_id left outer join
		 #mainactors mainactors on mainactors.movie_id = mc.movie_id left outer join
		 [dbo].[actors_dim] a on mc.actor_id = a.actor_id
group by m.movie_id


SELECT top 10 percent movie_id, release_date
into #blockbusters
FROM [dbo].[movies]
order by revenue desc

-- temp table that aggregating information about actors: number of blockbusters (top 10% by revenue) actor participated in,
-- average revenue of all movies actor participated in, total number of movies, info about last 5 years movies
select a.actor_id, a.name, a.gender
,count(m.movie_id) movies
,SUM(CASE WHEN 2017 - Year(m.release_date) < 5 THEN 1 ELSE 0 END) movies_5y
,SUM(CASE WHEN b.movie_id is null THEN 0 ELSE 1 END) blockbusters
,SUM(CASE WHEN 2017 - Year(b.release_date) < 5 THEN 1 ELSE 0 END) blockbusters_5y
,ROUND(AVG(CAST(m.revenue as float)),0) avg_revenue
,ROUND(AVG(CAST((CASE WHEN 2017 - Year(m.release_date) < 5 THEN m.revenue ELSE null END) as float)),0) avg_revenue_5y
into #actors_revenue_rates
from [dbo].[actors_dim] a inner join 
		[dbo].[movie_cast] mc on mc.actor_id = a.actor_id inner join 
		[dbo].[movies] m on m.movie_id = mc.movie_id  and m.revenue > 0  left outer join
		#blockbusters b on b.movie_id = mc.movie_id 	
group by a.actor_id, a.name, a.gender

-- temp table that aggregating information about directors: number of blockbusters (top 10% by revenue) director participated in,
-- average revenue of all movies director participated in, total number of movies, info about last 5 years movies
select c.crew_id, c.name, c.gender
,count(m.movie_id) movies
,SUM(CASE WHEN 2017 - Year(m.release_date) < 5 THEN 1 ELSE 0 END) movies_5y
,SUM(CASE WHEN b.movie_id is null THEN 0 ELSE 1 END) blockbusters
,SUM(CASE WHEN 2017 - Year(b.release_date) < 5 THEN 1 ELSE 0 END) blockbusters_5y
,ROUND(AVG(CAST(m.revenue as float)),0) avg_revenue
,ROUND(AVG(CAST((CASE WHEN 2017 - Year(m.release_date) < 5 THEN m.revenue ELSE null END) as float)),0) avg_revenue_5y
into #directors_revenue_rates
from [dbo].[crew_dim] c inner join 
		[dbo].[movie_crew] mc on mc.crew_id = c.crew_id and mc.job = 'Director' inner join 
		[dbo].[movies] m on m.movie_id = mc.movie_id and m.revenue > 0  left outer join
		#blockbusters b on b.movie_id = mc.movie_id
group by c.crew_id, c.name, c.gender

-- temp table that aggregating information about producers: number of blockbusters (top 10% by revenue) producer participated in,
-- average revenue of all movies producer participated in, total number of movies, info about last 5 years movies
select c.crew_id, c.name, c.gender
,count(m.movie_id) movies
,SUM(CASE WHEN 2017 - Year(m.release_date) < 5 THEN 1 ELSE 0 END) movies_5y
,SUM(CASE WHEN b.movie_id is null THEN 0 ELSE 1 END) blockbusters
,SUM(CASE WHEN 2017 - Year(b.release_date) < 5 THEN 1 ELSE 0 END) blockbusters_5y
,ROUND(AVG(CAST(m.revenue as float)),0) avg_revenue
,ROUND(AVG(CAST((CASE WHEN 2017 - Year(m.release_date) < 5 THEN m.revenue ELSE null END) as float)),0) avg_revenue_5y
into #producers_revenue_rates
from [dbo].[crew_dim] c inner join 
		[dbo].[movie_crew] mc on mc.crew_id = c.crew_id and mc.job = 'Producer' inner join 
		[dbo].[movies] m on m.movie_id = mc.movie_id and m.revenue > 0 left outer join
		#blockbusters b on b.movie_id = mc.movie_id
group by c.crew_id, c.name, c.gender

-- temp table contains aggregated info about actors in movie
select 
    m.movie_id,
    max(arr0.avg_revenue) actor0_avg_revenue,
	max(arr0.avg_revenue_5y) actor0_avg_revenue_5y,
	max(arr0.blockbusters) actor0_blockbusters,
	max(arr0.blockbusters_5y) actor0_blockbusters_5y,
	max(arr0.movies) actor0_movies,
	max(arr0.movies_5y) actor0_movies_5y,
	max(arr1.avg_revenue) actor1_avg_revenue,
	max(arr1.avg_revenue_5y) actor1_avg_revenue_5y,
	max(arr1.blockbusters) actor1_blockbusters,
	max(arr1.blockbusters_5y) actor1_blockbusters_5y,
	max(arr1.movies) actor1_movies,
	max(arr1.movies_5y) actor1_movies_5y,
	max(arr2.avg_revenue) actor2_avg_revenue,
	max(arr2.avg_revenue_5y) actor2_avg_revenue_5y,
	max(arr2.blockbusters) actor2_blockbusters,
	max(arr2.blockbusters_5y) actor2_blockbusters_5y,
	max(arr2.movies) actor2_movies,
	max(arr2.movies_5y) actor2_movies_5y,
	max(arr0.gender) actor0_gender,
	max(arr1.gender) actor1_gender,
	max(arr2.gender) actor2_gender
into #movies_actors_revenues
from [dbo].[movies] m left outer join 
	 #mainactors ma on ma.movie_id = m.movie_id left outer join
	 #actors_revenue_rates arr0 on arr0.actor_id = ma.actor0_id left outer join 
     #actors_revenue_rates arr1 on arr1.actor_id = ma.actor1_id left outer join 
	 #actors_revenue_rates arr2 on arr2.actor_id = ma.actor2_id 
group by m.movie_id



select 
m.movie_id,
ROUND(AVG(drr.avg_revenue), 1) all_directors_avg_revenue,  -- avg revenue for all movie directors
ROUND(AVG(drr.avg_revenue_5y), 1) all_directors_avg_revenue_5y,
ROUND(AVG(drr.blockbusters), 1) all_directors_avg_blockbusters,
ROUND(AVG(drr.blockbusters_5y), 1) all_directors_avg_blockbusters_5y
into #movies_directors_revenues
from [dbo].[movies] m inner join 
     [dbo].[movie_crew] mc on m.movie_id = mc.movie_id and mc.job = 'Director' inner join
     #directors_revenue_rates drr on drr.crew_id = mc.crew_id
group by m.movie_id


select 
m.movie_id,
ROUND(AVG(prr.avg_revenue), 1) all_producers_avg_revenue,  -- avg revenue for all movie producers
ROUND(AVG(prr.avg_revenue_5y), 1) all_producers_avg_revenue_5y,
ROUND(AVG(prr.blockbusters), 1) all_producers_avg_blockbusters,
ROUND(AVG(prr.blockbusters_5y), 1) all_producers_avg_blockbusters_5y
into #movies_producers_revenues
from [dbo].[movies] m inner join 
     [dbo].[movie_crew] mc on m.movie_id = mc.movie_id and mc.job = 'Producer' inner join
     #producers_revenue_rates prr on prr.crew_id = mc.crew_id
group by m.movie_id



select m.movie_id, count(*) crew_total_cnt,
SUM(case when mc.[department] = 'writing' THEN 1 ELSE 0 END) crew_writing_cnt,
SUM(case when mc.[department] = 'directing' THEN 1 ELSE 0 END) crew_directing_cnt,
SUM(case when mc.[department] = 'art' THEN 1 ELSE 0 END) crew_art_cnt,
SUM(case when mc.[department] = 'sound' THEN 1 ELSE 0 END) crew_sound_cnt,
SUM(case when mc.[department] = 'costume&makeup' THEN 1 ELSE 0 END) crew_costume_makeup_cnt,
SUM(case when mc.[department] = 'visual_effects' THEN 1 ELSE 0 END) crew_visual_effects_cnt,
SUM(case when mc.[department] = 'production' THEN 1 ELSE 0 END) crew_production_cnt,
SUM(case when mc.[department] = 'actors' THEN 1 ELSE 0 END) crew_actors_cnt,
SUM(case when mc.[department] = 'camera' THEN 1 ELSE 0 END) crew_camera_cnt,
SUM(case when mc.[department] = 'lighting' THEN 1 ELSE 0 END) crew_lighting_cnt,
SUM(case when mc.[department] = 'editing' THEN 1 ELSE 0 END) crew_editing_cnt
into #movies_crew
from [dbo].[movies] m inner join 
     [dbo].[movie_crew] mc on mc.movie_id = m.movie_id inner join 
	 [dbo].[crew_dim] c on c.crew_id = mc.crew_id
group by m.movie_id



select m.movie_id,
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
    mi.movie_id,
	mi.original_title,
    mi.revenue,
	mi.budget,
	mi.popularity,
	ma.actors_cnt,
    mi.original_language,
	mi.language_en,
	mi.language_fr,
	mi.language_es,
	mi.language_de,
	mi.language_ru,
	mi.language_it,
	mi.language_ja,
	mi.release_date,
	mi.release_year,
	mi.release_month,
	mi.release_week,
	mi.release_weekday,
	mi.runtime,
    mi.runtime_intervals,
    mi.runtime_cat,
	mk.is_collection,
	mk.keywords_cnt,

	mc.crew_total_cnt,
	mc.crew_writing_cnt,
	mc.crew_directing_cnt,
	mc.crew_art_cnt,
	mc.crew_sound_cnt,
	mc.crew_costume_makeup_cnt,
	mc.crew_visual_effects_cnt,
	mc.crew_production_cnt,
	mc.crew_actors_cnt,
	mc.crew_camera_cnt,
	mc.crew_lighting_cnt,
	mc.crew_editing_cnt,
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

	mar.actor0_avg_revenue,  
	mar.actor0_avg_revenue_5y,
	mar.actor0_blockbusters,
	mar.actor0_blockbusters_5y,
	mar.actor0_movies,
	mar.actor0_movies_5y,
	mar.actor0_gender,
	mar.actor1_avg_revenue,  
	mar.actor1_avg_revenue_5y,
	mar.actor1_blockbusters,
	mar.actor1_blockbusters_5y,
	mar.actor1_movies,
	mar.actor1_movies_5y,
	mar.actor1_gender,
	mar.actor2_avg_revenue,  
	mar.actor2_avg_revenue_5y,
	mar.actor2_blockbusters,
	mar.actor2_blockbusters_5y,
	mar.actor2_movies,
	mar.actor2_movies_5y,
	mar.actor2_gender,
  
	mdr.all_directors_avg_revenue,  
	mdr.all_directors_avg_blockbusters,
	mdr.all_directors_avg_revenue_5y,  
	mdr.all_directors_avg_blockbusters_5y,
 
	mpr.all_producers_avg_revenue, 
	mpr.all_producers_avg_blockbusters,
	mpr.all_producers_avg_revenue_5y, 
	mpr.all_producers_avg_blockbusters_5y

from #movies_info mi left outer join 
     #movies_actors ma on mi.movie_id = ma.movie_id left outer join
	 #movies_crew mc on mc.movie_id = ma.movie_id left outer join
	 #movies_genres mg on mg.movie_id = ma.movie_id left outer join
	 #movies_actors_revenues mar on mar.movie_id = ma.movie_id left outer join
	 #movies_directors_revenues mdr on mdr.movie_id = ma.movie_id left outer join
	 #movies_producers_revenues mpr on mpr.movie_id = ma.movie_id left outer join 
	 #movies_keywords mk on mk.movie_id = mi.movie_id
order by 1



IF OBJECT_ID('tempdb..#movies_info') IS NOT NULL DROP TABLE #movies_info
IF OBJECT_ID('tempdb..#movies_actors') IS NOT NULL DROP TABLE #movies_actors
IF OBJECT_ID('tempdb..#blockbusters') IS NOT NULL DROP TABLE #blockbusters
IF OBJECT_ID('tempdb..#actors_revenue_rates') IS NOT NULL DROP TABLE #actors_revenue_rates
IF OBJECT_ID('tempdb..#directors_revenue_rates') IS NOT NULL DROP TABLE #directors_revenue_rates
IF OBJECT_ID('tempdb..#producers_revenue_rates') IS NOT NULL DROP TABLE #producers_revenue_rates
IF OBJECT_ID('tempdb..#movies_crew') IS NOT NULL DROP TABLE #movies_crew
IF OBJECT_ID('tempdb..#movies_genres') IS NOT NULL DROP TABLE #movies_genres
IF OBJECT_ID('tempdb..#movies_actors_revenues') IS NOT NULL DROP TABLE #movies_actors_revenues
IF OBJECT_ID('tempdb..#movies_directors_revenues') IS NOT NULL DROP TABLE #movies_directors_revenues
IF OBJECT_ID('tempdb..#movies_producers_revenues') IS NOT NULL DROP TABLE #movies_producers_revenues
IF OBJECT_ID('tempdb..#mainactors') IS NOT NULL DROP TABLE #mainactors
IF OBJECT_ID('tempdb..#movies_keywords') IS NOT NULL DROP TABLE #movies_keywords