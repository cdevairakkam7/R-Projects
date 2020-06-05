


# Loading A1-1_pages.csv 
pages_dataset<-read.csv("https://www.dropbox.com/s/m8yjzsnwxs4ohbe/A1-1_pages.csv?dl=0",header =TRUE)

# Top 10 Visited pages
top_10_visited_pages<-sqldf("SELECT count(path) as Count,Path from pages_dataset group by 2 order by 1 desc limit 10")

# Re-Ordering
top_10_visited_pages$path<-factor(top_10_visited_pages$path,levels = c("/","/category/food","/shop_all","/category/home-and-office","/category/beauty","/category/personal-care","/category/household-supplies","/category/health","/about","/checkout/email"))

# Top 10 pages visited in a ggplot
ggplot(top_10_visited_pages,aes(x=path,y=Count,label=Count))+labs(title="Top 10 pages visited",x="Path",y="# of times visited")+theme(axis.text.x = element_text(angle =45,vjust = 0.5))+geom_bar(stat="identity", width = 0.5, aes(fill=path))+geom_text(hjust=0.09,angle=45)+theme(text=element_text(size=10, family="Comic Sans MS"))
