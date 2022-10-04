

library(tidyverse)
library(janitor)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)





##### Movie data frame
 
movie_tomato_scores_long_chart <- ggplot(movie_sans_outlier_df, aes(x=release_year_movie, y=rotten_tomatoes_movie, color=all_platforms_movie))+
  geom_smooth()+
  labs(y = "Rotten Tomatoes rating", x = "Film release year", color= "Movie streaming platforms")+
  ggtitle("Rotten Tomatoes scores over time for films")
movie_tomato_scores_long_chart
 

##### TV data frame
 
tv_tomato_scores_long_chart <- ggplot(tv_sans_outlier_df, aes(x=release_year_tv, y=rotten_tomatoes_tv, color=all_platforms_tv))+
  geom_smooth()+
  labs(y = "Rotten Tomatoes rating", x = "Show release year", color= "TV streaming platforms")+
  ggtitle("Rotten Tomatoes ratings over time for TV shows")
tv_tomato_scores_long_chart
 


#### Rotten Tomatoes distribution for all(historical) and recent (2020-2021) content
#Is there a difference in those films and shows that have been recently released (released between 2020-2021) and the total content available on each platform? To explore this, we will examine historical data and then make quick data frames to account for only the most recent films and shows. As platforms have been releasing original content (Netflix has "Netflix Originals", Hulu has "Hulu Originals", Prime Video has "Amazon Originals", and Disney+ has "Disney Originals") in recent years, this could be valuable insight. We will take a count of the content released, them examine the percentages of each platform.

##### Both movie data frame and tv data frame

historical_total_tomato_count_chart<- 
  ggplot() + 
  geom_bar(data = movie_df_3, aes(x = all_platforms_movie, fill=tomato_category_movie )) +
  theme(axis.text.x = element_text(angle= 45,  hjust = 1, vjust= 1))+
  scale_fill_manual(values= c("certified_fresh"= "green4", "fresh"="deepskyblue4", "rotten"="gold"))+
  labs(y = "Content count", x = "Streaming platform", fill= "Rotten Tomato category")+
  ggtitle("Total content count of Rotten Tomatoes categories for films and TV shows")+
  geom_bar(data = tv_df_3, aes(x = all_platforms_tv, fill= tomato_category_tv)) 
historical_total_tomato_count_chart



movie_recent_df <- movie_df_3
movie_recent_df <- movie_recent_df %>% 
  filter(release_year_movie >= 2020)
tv_recent_df<- tv_df_3
tv_recent_df <- tv_recent_df %>% 
  filter(release_year_tv >= 2020)

recent_total_tomato_count_chart<- 
  ggplot() + 
  geom_bar(data = movie_recent_df, aes(x = all_platforms_movie, fill=tomato_category_movie )) +
  theme(axis.text.x = element_text(angle= 45,  hjust = 1, vjust= 1))+
  scale_fill_manual(values= c("certified_fresh"= "green4", "fresh"="deepskyblue4", "rotten"="gold"))+
  labs(y = "Content count", x = "Streaming platform", fill= "Rotten Tomato category")+
  ggtitle("Total content count of Rotten Tomatoes categories for recent films and TV shows, 2020-2021")+
  geom_bar(data = tv_recent_df, aes(x = all_platforms_tv, fill= tomato_category_tv)) 
recent_total_tomato_count_chart



##### Movie data frame


movie_percent_chart_df <- movie_df_3
movie_percent_chart_df <- movie_percent_chart_df %>% 
  drop_na(tomato_category_movie) %>% 
  count(all_platforms_movie, tomato_category_movie) %>% 
  group_by(all_platforms_movie)
movie_percent_chart_df$new_row_movie<- movie_percent_chart_df %>% 
  mutate(prop= n/sum(n),
         tomato_category_movie= factor(tomato_category_movie, levels = 1:3, labels = c("certified_fresh", "fresh", "rotten")))
movie_historic_tomato_distribution_chart_percent_labeled <- ggplot(movie_percent_chart_df, aes(x=all_platforms_movie, y=new_row_movie$prop, fill= tomato_category_movie))+
  geom_col()+
  geom_text(aes(label = percent(new_row_movie$prop)), position = position_stack(vjust = .5))+
  scale_fill_manual(values= c("certified_fresh"= "green4", "fresh"="deepskyblue4", "rotten"="gold"))+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle= 45,  hjust = 1, vjust= 1))+
  labs(y = "Distribution percentage", x = "Streaming platform", fill= "Rotten Tomato category") +
  ggtitle("Total distribution of Rotten Tomatoes categories over time for films")
movie_historic_tomato_distribution_chart_percent_labeled 
 


movie_recent_percent_chart_df <- movie_recent_df
movie_recent_percent_chart_df <- movie_recent_percent_chart_df %>% 
  drop_na(tomato_category_movie) %>% 
  count(all_platforms_movie, tomato_category_movie) %>% 
  group_by(all_platforms_movie)
movie_recent_percent_chart_df$new_row_movie<- movie_recent_percent_chart_df %>% 
  mutate(prop= n/sum(n),
         tomato_category_movie= factor(tomato_category_movie, levels = 1:3, labels = c("certified_fresh", "fresh", "rotten")))
movie_recent_tomato_distribution_chart_percent_labeled <- ggplot(movie_recent_percent_chart_df, aes(x=all_platforms_movie, y=new_row_movie$prop, fill= tomato_category_movie))+
  geom_col()+
  geom_text(aes(label = percent(new_row_movie$prop)), position = position_stack(vjust = .5))+
  scale_fill_manual(values= c("certified_fresh"= "green4", "fresh"="deepskyblue4", "rotten"="gold"))+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle= 45,  hjust = 1, vjust= 1))+
  labs(y = "Distribution percentage", x = "Streaming platform", fill= "Rotten Tomato category") +
  ggtitle("Recent distribution of Rotten Tomatoes categories for films, 2020-2021")
movie_recent_tomato_distribution_chart_percent_labeled 



##### TV data frame


tv_percent_chart_df <- tv_df_3
tv_percent_chart_df <- tv_percent_chart_df %>% 
  drop_na(tomato_category_tv) %>% 
  count(all_platforms_tv, tomato_category_tv) %>% 
  group_by(all_platforms_tv)
tv_percent_chart_df$new_row_tv<- tv_percent_chart_df %>% 
  mutate(prop= n/sum(n),
         tomato_category_tv= factor(tomato_category_tv, levels = 1:3, labels = c("certified_fresh", "fresh", "rotten")))
tv_historic_tomato_distribution_chart_percent_labeled <- ggplot(tv_percent_chart_df, aes(x=all_platforms_tv, y=new_row_tv$prop, fill= tomato_category_tv))+
  geom_col()+
  geom_text(aes(label = percent(new_row_tv$prop)), position = position_stack(vjust = .5))+
  scale_fill_manual(values= c("certified_fresh"= "green4", "fresh"="deepskyblue4", "rotten"="gold"))+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle= 45,  hjust = 1, vjust= 1))+
  labs(y = "Distribution percentage", x = "Streaming platform", fill= "Rotten Tomato category") +
  ggtitle("Total distribution of Rotten Tomatoes categories over time for TV shows")
tv_historic_tomato_distribution_chart_percent_labeled 


tv_recent_percent_chart_df <- tv_recent_df
tv_recent_percent_chart_df <- tv_recent_percent_chart_df %>% 
  drop_na(tomato_category_tv) %>% 
  count(all_platforms_tv, tomato_category_tv) %>% 
  group_by(all_platforms_tv)
tv_recent_percent_chart_df$new_row_tv<- tv_recent_percent_chart_df %>% 
  mutate(prop= n/sum(n),
         tomato_category_tv= factor(tomato_category_tv, levels = 1:3, labels = c("certified_fresh", "fresh", "rotten")))
tv_recent_tomato_distribution_chart_percent_labeled <- ggplot(tv_recent_percent_chart_df, aes(x=all_platforms_tv, y=new_row_tv$prop, fill= tomato_category_tv))+
  geom_col()+
  geom_text(aes(label = percent(new_row_tv$prop)), position = position_stack(vjust = .5))+
  scale_fill_manual(values= c("certified_fresh"= "green4", "fresh"="deepskyblue4", "rotten"="gold"))+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle= 45,  hjust = 1, vjust= 1))+
  labs(y = "Distribution percentage", x = "Streaming platform", fill= "Rotten Tomato category") +
  ggtitle("Recent distribution of Rotten Tomatoes categories for TV shows, 2020-2021")
tv_recent_tomato_distribution_chart_percent_labeled 



#### Age category distribution
#Now we can look into our other line of inquiry, the age ranges that each platform has.First, we're going to have the age categories appear in our desire order, least inclusive to most, then make a chart.

##### Movie data frame

movie_age_percent_chart_df <- movie_df_3
movie_age_percent_chart_df <- movie_age_percent_chart_df %>% 
  drop_na(age_rating_movie) %>% 
  count(all_platforms_movie, age_rating_movie) %>% 
  group_by(all_platforms_movie) 

movie_age_percent_chart_df$new_row <- movie_age_percent_chart_df %>% 
  mutate(prop = n/sum(n),
         age_rating_movie = factor(age_rating_movie, levels=(1:6), labels = c("unknown", "18+", "16+", "13+", "7+", "all")))
movie_age_percent_chart_df$age_rating_movie <- factor(movie_age_percent_chart_df$age_rating_movie, levels= c("unknown", "18+", "16+", "13+", "7+", "all"))
movie_age_rating_distribution_chart_percent_labeled <- ggplot(movie_age_percent_chart_df, aes(x=all_platforms_movie, y=new_row$prop, fill=age_rating_movie))+
  geom_col()+
  geom_text(aes(label = percent(new_row$prop)), position = position_stack(vjust = .5)) +
  scale_fill_manual(values = c("unknown"="firebrick2", "18+"="green4", "16+"="gold","13+"="deeppink4", "7+"="deepskyblue4", "all"="darkorange2")) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle= 45,  hjust = 1, vjust= 1))+
  labs(y = "Distribution percentage", x = "Streaming platform", fill= "Age rating") +
  ggtitle("All age rating distribution per platform for films")
movie_age_rating_distribution_chart_percent_labeled 




movie_known_age_percent_chart_df <- movie_df_3 %>% 
  filter(age_rating_movie != "unknown")

movie_known_age_percent_chart_df <- movie_known_age_percent_chart_df %>% 
  drop_na(age_rating_movie) %>% 
  count(all_platforms_movie, age_rating_movie) %>% 
  group_by(all_platforms_movie) 
movie_known_age_percent_chart_df$new_row_movie <- movie_known_age_percent_chart_df %>% 
  mutate(prop = n/sum(n),
         age_rating_movie= factor(age_rating_movie, levels = 1:5, labels = c("18+", "16+", "13+", "7+", "all")))
movie_known_age_percent_chart_df$age_rating_movie <- factor(movie_known_age_percent_chart_df$age_rating_movie, levels= c("18+", "16+", "13+", "7+", "all"))
movie_known_age_rating_distribution_chart_percent_labeled <- ggplot(movie_known_age_percent_chart_df, aes(x=all_platforms_movie, y=new_row_movie$prop, fill=age_rating_movie))+
  geom_col()+
  geom_text(aes(label = percent(new_row_movie$prop)), position = position_stack(vjust = .5)) +
  scale_fill_manual(values = c("18+"="green4", 
                               "16+"="gold", 
                               "13+"="deeppink4", 
                               "7+"="deepskyblue4", 
                               "all"="darkorange2")) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle= 45,  hjust = 1, vjust= 1))+
  labs(y = "Distribution percentage", x = "Streaming platform", fill= "Age rating") +
  ggtitle("Known age rating distribution per platform for films")
movie_known_age_rating_distribution_chart_percent_labeled 


##### TV data frame

tv_age_percent_chart_df <- tv_df_3
tv_age_percent_chart_df <- tv_age_percent_chart_df %>% 
  drop_na(age_rating_tv) %>% 
  count(all_platforms_tv, age_rating_tv) %>% 
  group_by(all_platforms_tv) 
tv_age_percent_chart_df$new_row <- tv_age_percent_chart_df %>% 
  mutate(prop = n/sum(n),
         age_rating_tv= factor(age_rating_tv, levels = 1:6, labels = c("unknown", "18+", "16+", "13+", "7+", "all")))
tv_age_percent_chart_df$age_rating_tv <- factor(tv_age_percent_chart_df$age_rating_tv, levels= c("unknown", "18+", "16+", "13+", "7+", "all"))
tv_age_rating_distribution_chart_percent_labeled <- ggplot(tv_age_percent_chart_df, aes(x=all_platforms_tv, y=new_row$prop, fill=age_rating_tv))+
  geom_col()+
  geom_text(aes(label = percent(new_row$prop)), position = position_stack(vjust = .5)) +
  scale_fill_manual(values = c("unknown" = "firebrick2", 
                               "18+"="green4", 
                               "16+"="gold", 
                               "13+"="deeppink4", 
                               "7+"="deepskyblue4", 
                               "all"="darkorange2")) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle= 45,  hjust = 1, vjust= 1))+
  labs(y = "Distribution percentage", x = "Streaming platform", fill= "Age rating") +
  ggtitle("All age rating distribution per platform for TV shows")
tv_age_rating_distribution_chart_percent_labeled




tv_known_age_percent_chart_df <- tv_df_3 %>% 
  filter(age_rating_tv != "unknown")

tv_known_age_percent_chart_df <- tv_known_age_percent_chart_df %>% 
  drop_na(age_rating_tv) %>% 
  count(all_platforms_tv, age_rating_tv) %>% 
  group_by(all_platforms_tv) 
tv_known_age_percent_chart_df$new_row <- tv_known_age_percent_chart_df %>% 
  mutate(prop = n/sum(n),
         age_rating_tv= factor(age_rating_tv, levels = 1:5, labels = c("18+", "16+", "13+", "7+", "all")))

tv_known_age_percent_chart_df$age_rating_tv <- factor(tv_known_age_percent_chart_df$age_rating_tv, levels = c("18+", "16+", "13+", "7+", "all"))

tv_known_age_rating_distribution_chart_percent_labeled <- ggplot(tv_known_age_percent_chart_df, aes(x=all_platforms_tv, y=new_row$prop, fill=age_rating_tv))+
  geom_col()+
  geom_text(aes(label = percent(new_row$prop)), position = position_stack(vjust = .5)) +
  scale_fill_manual(values = c("18+"="green4", 
                               "16+"="gold", 
                               "13+"="deeppink4", 
                               "7+"="deepskyblue4", 
                               "all"="darkorange2")) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle= 45,  hjust = 1, vjust= 1))+
  labs(y = "Distribution percentage", x = "Streaming platform", fill= "Age rating") +
  ggtitle("Known age rating distribution per platform for TV shows")
tv_known_age_rating_distribution_chart_percent_labeled



### Step 6. Sharing Findings
#### Business task
#The business task set to us by our fictional TV company was to help them prioritize their software development by identifying Which streaming platform offers the highest quality content, as rated by Rotten Tomatoes, and which has the widest viewing age range. Our visualizations and analysis layout the data needed to answer these questions. again, the streaming platforms we are examining are: Netflix, Hulu, Prime Video, and Disney+ . 

#### Content quantity versus quality
#There are a few angles to examine for content quality. We'll begin by examining historical data and content rating distribution.
#### Mean Rotten Tomatoes for each platform

##### Movie data frame
 
netflix_movie_rt_mean <- aggregate(movie_df_3$rotten_tomatoes_movie, list(movie_df_3$netflix_movie), mean)
hulu_movie_rt_mean <- aggregate(movie_df_3$rotten_tomatoes_movie, list(movie_df_3$hulu_movie), mean)
prime_video_moive_rt_mean <- aggregate(movie_df_3$rotten_tomatoes_movie, list(movie_df_3$prime_video_movie), mean)
disney_movie_rt_mean <- aggregate(movie_df_3$rotten_tomatoes_movie, list(movie_df_3$disney_movie), mean)


 
netflix_movie_rt_mean
hulu_movie_rt_mean
prime_video_moive_rt_mean
disney_movie_rt_mean



#Disney is 58.31236, Netflix is 54.44794, Hulu is 60.39733, Prime Video is 50.39874, Disney is 58.31236.  We can see here that Hulu has the highest average rating, with Disney scoring closely, and Netflix and Prime scoring lower. 

##### TV data frame
 
netflix_tv_rt_mean <- aggregate(tv_df_3$rotten_tomatoes_tv, list(tv_df_3$netflix_tv), mean)
hulu_tv_rt_mean <- aggregate(tv_df_3$rotten_tomatoes_tv, list(tv_df_3$hulu_tv), mean)
prime_tv_moive_rt_mean <- aggregate(tv_df_3$rotten_tomatoes_tv, list(tv_df_3$prime_video_tv), mean)
disney_tv_rt_mean <- aggregate(tv_df_3$rotten_tomatoes_tv, list(tv_df_3$disney_tv), mean)
 

 
netflix_tv_rt_mean
hulu_tv_rt_mean 
prime_tv_moive_rt_mean 
disney_tv_rt_mean 


#Disney is 49.42450, Netflix is 53.55911, Hulu is 52.83775, and Prime Video is 37.76133. We can see here that Netflix has the highest average rating, with Hulu scoring closely, Disney in third, and Prime Video scoring far lower. 

##### Historical Rotten Tomatoes scores
 
figure_1_visual <- grid.arrange(top= "Figure 1. Rotten Tomatoes scores over time", 
             movie_tomato_scores_long_chart, tv_tomato_scores_long_chart)


#Historically for films, there is a recent plummet in content quality across all platforms, as seen in Figure 1 above. Disney+ takes a noticeable dip and Prime Video has a low, but steady rating. Historic data for TV shows have an intriguing upswing for Prime Video scoring near 65 and a meeting of all other platforms with scores between 50 and 60. Overall, movies have a decline and most TV shows are meeting to similar scores.

##### Quantity versus quality for across content type (films and shows)
 
figure_2_visual <- grid.arrange(top= "Figure 2.", historical_total_tomato_count_chart)


#The figures above show the total content available on all platforms. It is clear that Prime Video has the most content, but also has the most rotten-scoring content, with Netflix close behind. Disney+ and Hulu look similar in their film rotten, fresh, and certified fresh content, but they also do not have nearly as much content as Prime Video or Netflix. the business task want to know about quality of content, so we'll take a quick look at recent content and then examine percent distribution of content ratings.  

  
figure_3_visual <-grid.arrange(top="Figure 3.", recent_total_tomato_count_chart)


#We can see here that Netflix has been producing the most in terms of recent content, with the majority of it rating rotten. Prime Video is second, with the majority also rating rotten. Hulu has the best distribution for it's new content, but much less. These graph are important for, although we're interested in the highest quality of content, rather than quantity of content, there is a connection emerging; those platforms that have more content tend to have a large amount rated rotten. Now we'll examine percent distribution. 


##### Rotten Tomatoes distribution


figure_4_visual <- grid.arrange(top = "Figure 4. Film distribution of Rotten Tomatoes score",
             movie_historic_tomato_distribution_chart_percent_labeled , movie_recent_tomato_distribution_chart_percent_labeled )


#Figure 4. above shows that all platforms contain at least 47% rotten films; nearly half of all films are scoring a 50 or lower with rotten tomatoes. When you look at recent films, all platforms have an increase in rotten content. Aside from multi-platform films, Hulu has the least percentage of rotten films historically and recently; Hulu also historically has highest percent of fresh films. Recently, Hulu has the least rotten films and the most fresh films. Disney+ has the highest percent of fresh films historically and recently. This figure indicated that Hulu is emerging as having the best overall quality films, with Disney+ have the most top quality films.


figure_5_visual <- grid.arrange(top = "Figure 5. TV show distribution of Rotten Tomatoes score",
             tv_historic_tomato_distribution_chart_percent_labeled , tv_recent_tomato_distribution_chart_percent_labeled )


#Figure 5. shows that platforms have a higher rotten percent distribution for shows than for films, aside form multi-platform shows. Historically, it seems that multi-platform shows hve the best rated content. Hulu is in second, with over 66% of it's content rotten. Recently, however, it looks that Prime Video is producing higher quality content. 

#### Age appropriateness recommendations

#Next, we'll take a look into the age appropriateness for content across various platforms. This gives us an idea of what age ranges each platform appeals to. On that has more content for all ages would be ideal for a family, while those rated with a higher age recommendation maybe more appropriate for an audience without children. 

 
figure_6_visual <- grid.arrange(top="Figure 6. All age range distribution for all content across all platforms", movie_age_rating_distribution_chart_percent_labeled , tv_age_rating_distribution_chart_percent_labeled, ncol=2)


#Figure 6. above shows all age ratings, including those that have an unknown rating. Prime Video films have nearly 48% of their films rated as known, meaning that if consumers have children, age appropriateness of content could be problematic. Disney+ stands out in films and shows as having a over 50% of their content for children aged under 13 years old. 



figure_7_visual <- grid.arrange(top="Figure 7. Known age range distribution for all content across all platforms", movie_known_age_rating_distribution_chart_percent_labeled  , tv_known_age_rating_distribution_chart_percent_labeled, ncol=2)




