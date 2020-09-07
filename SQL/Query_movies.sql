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




