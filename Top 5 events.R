# Loading A1-1_tracks.csv 
tracks_dataset<-read.csv("https://www.dropbox.com/s/1jidkbtquvlg3fb/A1-1_tracks.csv?dl=0",header =TRUE)

# Joining Tracks and Pages
tracks_and_pages_joined<-sqldf("SELECT count(a.event) as count, a.event as event, b.path FROM tracks_dataset a INNER JOIN top_10_visited_pages b ON a.context_page_path=b.path GROUP BY 2,3 order by count desc,path")

# Grouping and Ranking
tracks_and_pages_Rank<-tracks_and_pages_joined %>% group_by(path) %>% mutate(ranks=order(count,decreasing = TRUE))

# Top 5 tracks from Top 10 most visited pages
top_5_tracks<-filter(tracks_and_pages_Rank,ranks<6)

# Re-Ordering
top_5_tracks<-top_5_tracks %>% group_by(path) %>% arrange(desc(count),.by_group = TRUE)

# Visualizing Top 5 Events in Top 10 pages Visited 
ggplot(top_5_tracks,aes(x=path,y=count,fill=event))+geom_bar(stat = "identity",position="fill")+theme(axis.text.x = element_text(angle =45,vjust = 0.5))+theme(text=element_text(size=10, family="Comic Sans MS"))+scale_fill_brewer(palette="Spectral")+labs(title="Top 5 Events Tracked",x="Path",y="# of times events Clicked")
