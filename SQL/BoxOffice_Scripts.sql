USE [BoxOffice]
GO
/****** Object:  Table [dbo].[actors_dim]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[actors_dim](
	[actor_id] [int] NOT NULL,
	[name] [nvarchar](250) NULL,
	[gender] [int] NULL,
	[profile_path] [nvarchar](255) NULL,
PRIMARY KEY CLUSTERED 
(
	[actor_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
/****** Object:  Table [dbo].[collection_dim]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[collection_dim](
	[collection_id] [int] NOT NULL,
	[name] [varchar](max) NULL,
	[poster_path] [varchar](max) NULL,
	[backdrop_path] [varchar](max) NULL,
PRIMARY KEY CLUSTERED 
(
	[collection_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[countries_dim]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[countries_dim](
	[iso_3166_1] [nvarchar](3) NOT NULL,
	[name] [nvarchar](250) NULL,
PRIMARY KEY CLUSTERED 
(
	[iso_3166_1] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
/****** Object:  Table [dbo].[crew_dim]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[crew_dim](
	[crew_id] [int] NOT NULL,
	[gender] [int] NULL,
	[name] [nvarchar](250) NULL,
	[profile_path] [nvarchar](250) NULL,
PRIMARY KEY CLUSTERED 
(
	[crew_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
/****** Object:  Table [dbo].[genres_dim]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[genres_dim](
	[genre_id] [int] NOT NULL,
	[name] [nvarchar](250) NULL,
PRIMARY KEY CLUSTERED 
(
	[genre_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
/****** Object:  Table [dbo].[keywords_dim]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[keywords_dim](
	[keyword_id] [int] NOT NULL,
	[name] [varchar](max) NULL,
PRIMARY KEY CLUSTERED 
(
	[keyword_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[movie_cast]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[movie_cast](
	[cast_id] [int] NULL,
	[character] [nvarchar](max) NULL,
	[credit_id] [nvarchar](50) NULL,
	[actor_id] [int] NULL,
	[movie_id] [int] NULL,
	[order] [int] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
/****** Object:  Table [dbo].[movie_collection]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[movie_collection](
	[collection_id] [int] NULL,
	[movie_id] [int] NULL
) ON [PRIMARY]

GO
/****** Object:  Table [dbo].[movie_countries]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[movie_countries](
	[iso_3166_1] [nvarchar](3) NULL,
	[movie_id] [int] NULL
) ON [PRIMARY]

GO
/****** Object:  Table [dbo].[movie_crew]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[movie_crew](
	[credit_id] [nvarchar](250) NOT NULL,
	[department] [nvarchar](250) NULL,
	[crew_id] [int] NULL,
	[job] [nvarchar](250) NULL,
	[movie_id] [int] NULL,
PRIMARY KEY CLUSTERED 
(
	[credit_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
/****** Object:  Table [dbo].[movie_keywords]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[movie_keywords](
	[keyword_id] [int] NULL,
	[movie_id] [int] NULL
) ON [PRIMARY]

GO
/****** Object:  Table [dbo].[movie_languages]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[movie_languages](
	[iso_639_1] [nvarchar](3) NULL,
	[movie_id] [int] NULL,
	[sw_original_lang] [int] NULL
) ON [PRIMARY]

GO
/****** Object:  Table [dbo].[movie_overview]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[movie_overview](
	[movie_id] [int] NULL,
	[overview] [nvarchar](max) NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
/****** Object:  Table [dbo].[movie_productors]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[movie_productors](
	[productor_id] [int] NULL,
	[movie_id] [int] NULL
) ON [PRIMARY]

GO
/****** Object:  Table [dbo].[movies]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[movies](
	[movie_id] [int] NOT NULL,
	[budget] [int] NULL,
	[homepage] [nvarchar](max) NULL,
	[imdb_id] [nvarchar](100) NULL,
	[original_language] [nvarchar](3) NULL,
	[original_title] [nvarchar](max) NULL,
	[popularity] [float] NULL,
	[poster_path] [nvarchar](max) NULL,
	[release_date] [date] NULL,
	[runtime] [float] NULL,
	[status] [nvarchar](20) NULL,
	[tagline] [nvarchar](max) NULL,
	[revenue] [int] NULL,
PRIMARY KEY CLUSTERED 
(
	[movie_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
/****** Object:  Table [dbo].[movies_genres]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[movies_genres](
	[genre_id] [int] NULL,
	[movie_id] [int] NULL
) ON [PRIMARY]

GO
/****** Object:  Table [dbo].[productors_dim]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[productors_dim](
	[productor_id] [int] NOT NULL,
	[name] [nvarchar](250) NULL,
PRIMARY KEY CLUSTERED 
(
	[productor_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
/****** Object:  Table [dbo].[spoken_lang_dim]    Script Date: 9/9/2020 7:22:26 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[spoken_lang_dim](
	[iso_639_1] [nvarchar](3) NOT NULL,
	[name] [nvarchar](250) NULL,
PRIMARY KEY CLUSTERED 
(
	[iso_639_1] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
ALTER TABLE [dbo].[movie_cast]  WITH CHECK ADD FOREIGN KEY([actor_id])
REFERENCES [dbo].[actors_dim] ([actor_id])
GO
ALTER TABLE [dbo].[movie_cast]  WITH CHECK ADD FOREIGN KEY([movie_id])
REFERENCES [dbo].[movies] ([movie_id])
GO
ALTER TABLE [dbo].[movie_collection]  WITH CHECK ADD FOREIGN KEY([collection_id])
REFERENCES [dbo].[collection_dim] ([collection_id])
GO
ALTER TABLE [dbo].[movie_collection]  WITH CHECK ADD FOREIGN KEY([movie_id])
REFERENCES [dbo].[movies] ([movie_id])
GO
ALTER TABLE [dbo].[movie_countries]  WITH CHECK ADD FOREIGN KEY([iso_3166_1])
REFERENCES [dbo].[countries_dim] ([iso_3166_1])
GO
ALTER TABLE [dbo].[movie_countries]  WITH CHECK ADD FOREIGN KEY([movie_id])
REFERENCES [dbo].[movies] ([movie_id])
GO
ALTER TABLE [dbo].[movie_crew]  WITH CHECK ADD FOREIGN KEY([crew_id])
REFERENCES [dbo].[crew_dim] ([crew_id])
GO
ALTER TABLE [dbo].[movie_crew]  WITH CHECK ADD FOREIGN KEY([movie_id])
REFERENCES [dbo].[movies] ([movie_id])
GO
ALTER TABLE [dbo].[movie_keywords]  WITH CHECK ADD FOREIGN KEY([keyword_id])
REFERENCES [dbo].[keywords_dim] ([keyword_id])
GO
ALTER TABLE [dbo].[movie_keywords]  WITH CHECK ADD FOREIGN KEY([movie_id])
REFERENCES [dbo].[movies] ([movie_id])
GO
ALTER TABLE [dbo].[movie_languages]  WITH CHECK ADD FOREIGN KEY([iso_639_1])
REFERENCES [dbo].[spoken_lang_dim] ([iso_639_1])
GO
ALTER TABLE [dbo].[movie_languages]  WITH CHECK ADD FOREIGN KEY([movie_id])
REFERENCES [dbo].[movies] ([movie_id])
GO
ALTER TABLE [dbo].[movie_overview]  WITH CHECK ADD FOREIGN KEY([movie_id])
REFERENCES [dbo].[movies] ([movie_id])
GO
ALTER TABLE [dbo].[movie_productors]  WITH CHECK ADD FOREIGN KEY([movie_id])
REFERENCES [dbo].[movies] ([movie_id])
GO
ALTER TABLE [dbo].[movie_productors]  WITH CHECK ADD FOREIGN KEY([productor_id])
REFERENCES [dbo].[productors_dim] ([productor_id])
GO
ALTER TABLE [dbo].[movies_genres]  WITH CHECK ADD FOREIGN KEY([genre_id])
REFERENCES [dbo].[genres_dim] ([genre_id])
GO
ALTER TABLE [dbo].[movies_genres]  WITH CHECK ADD FOREIGN KEY([movie_id])
REFERENCES [dbo].[movies] ([movie_id])
GO
