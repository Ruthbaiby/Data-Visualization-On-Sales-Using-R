# http://docs.ggplot2.org/0.9.3.1/geom_bar.html
# https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
# http://docs.ggplot2.org/current/scale_brewer.html
# http://stackoverflow.com/questions/16074440/r-ggplot2-center-align-a-multi-line-title
# http://docs.ggplot2.org/current/scale_brewer.html
# https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
# https://www.r-bloggers.com/ggplot2-quick-heatmap-plotting/
# http://blog.qiubio.com:8080/archives/2477
# http://www.geo.ut.ee/aasa/LOOM02331/heatmap_in_R.html
# http://stackoverflow.com/questions/24265652/label-minimum-and-maximum-of-scale-fill-gradient-legend-with-text-ggplot2
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
# reference : https://www.kaggle.com/vadimnazarov/d/gregorut/videogamesales/exploratory-data-analysis-of-video-game-sales/comments
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(gridExtra)
library(scales)
 
df= read.csv("C:/Users/USER/Desktop/Data Visualization/vgsales.csv/vgsales.csv")
head(df)
summary(df)

# Platform count
platform = df %>% group_by(Platform) %>% summarise(Count = n())
p1 = ggplot(aes(x = Platform , y = Count , fill=Count) , data=platform) +
  geom_bar(colour='black',stat='identity') +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust=1) , 
        plot.title = element_text(hjust=0.5))+  ## title center
  ggtitle('Platform Count')+
  scale_fill_distiller(palette = 'RdYlBu') +
  ylab('Count')
grid.arrange(p1, ncol = 1)

#Platform Sales
platform_sales = df %>% group_by(Platform) %>% summarise(Global_Sales = sum(Global_Sales),
                                                         NA_Sales = sum(NA_Sales),
                                                         EU_Sales = sum(EU_Sales),
                                                         JP_Sales = sum(JP_Sales))

platform_sales = melt(platform_sales)
names(platform_sales) = c('Platform','SaleType','Sale')
ggplot(data = platform_sales,aes(x = Platform ,y = Sale , fill = SaleType)) + 
  geom_bar(colour='black',stat='identity',position='dodge') + 
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust=1),
        plot.title = element_text(hjust=0.5))+
  ggtitle('Platform Sales') +
  scale_fill_brewer(palette = 'YlGnBu')

# Genre Sales
genre_sales = df %>% group_by(Genre) %>% summarise(GlobalSales = sum(Global_Sales),
                                                   NA_Sales = sum(NA_Sales),
                                                   EU_Sales = sum(EU_Sales),
                                                   JP_Sales = sum(JP_Sales)) 
genre_sales = melt(genre_sales)
names(genre_sales) = c('Genre','SaleType','Sale')

ggplot(data=genre_sales,aes(x = Genre,y = Sale,fill=SaleType)) + 
  geom_bar(colour='black' , stat='identity', position='dodge') +  
  theme_bw()+
  theme(axis.text.x = element_text(hjust=1,angle=45),
        plot.title = element_text(hjust=0.5)) + ## center 
  ggtitle('Gener Sale') + 
  scale_fill_brewer(palette = 'YlGnBu')+
  ylab('Sale')

# Genre Count
genre_count = df %>% group_by(Genre) %>% summarise(Count = n())

ggplot(data=genre_count , aes(x = Genre,y=Count,fill=Count)) +
  geom_bar(colour='black',stat='identity') +
  theme_bw()+
  ggtitle('Genre Count') + 
  theme(axis.text.x = element_text(angle=45,hjust=1),
        plot.title = element_text(hjust=0.5)) +
  scale_fill_distiller(palette = 'RdYlBu') + 
  ylab('Count') 

# Release per year
df = filter(df,!is.na(Year))
year_sales = df %>%  group_by(Year) %>% summarise(Count = n()) 

ggplot(data=year_sales,aes(x=Year,y=Count,fill=Count)) + 
  geom_bar(colour='black',stat='identity') + 
  theme_bw()+
  ggtitle('Release per year') +
  theme(axis.text.x = element_text(angle=45,hjust=1),
        plot.title = element_text(hjust=0.5)) + 
  scale_fill_distiller(palette = 'YlGnBu')

# Top 20 Publisher
top_publisher = df %>% group_by(Publisher) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% top_n(20)

ggplot(data=top_publisher,aes(x=Publisher,y=Count,fill=Count)) +
  geom_bar(colour='black',stat='identity') + 
  theme_bw() +
  ggtitle('Top 20 Publisher') + 
  theme(axis.text.x = element_text(angle=45,hjust=1),
        plot.title = element_text(hjust=0.5)) + 
  scale_fill_distiller(palette = 'RdYlBu') + 
  coord_flip() ## coordinate flip bar == > barh 

# Most popular genre per year
popular_genre_per_year = df %>% group_by(Year,Genre) %>% 
  summarise(GlobalSales = sum(Global_Sales)) %>%
  arrange(desc(GlobalSales)) %>%
  arrange(Year) %>% top_n(1)

ggplot(data = popular_genre_per_year , 
       aes(x = Year, y = GlobalSales,fill=Genre)) +
  geom_bar(colour='black',stat='identity') +
  ggtitle('Most popular genre per year') +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,hjust=1),
        plot.title = element_text(hjust=.5)) +
  scale_fill_brewer(palette = 'RdYlBu')

#Platform per year
platform = df %>% group_by(Platform,Year) %>%
  summarize(TotalSales = sum(Global_Sales)) %>%
  arrange(desc(TotalSales))

ggplot(data=platform,aes(x = Year,y=TotalSales,colour=Platform,group=Platform)) + 
  geom_line(size=.2) + 
  geom_point(size=.2)+
  ggtitle('Platform per year') + 
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust=1),
        plot.title = element_text(hjust=0.5)) 

# Genre per year
genre = df %>% group_by(Genre,Year) %>%
  summarise(Total_Sales = sum(Global_Sales)) %>%
  arrange(desc(Total_Sales))
ggplot(data=genre,aes(x = Year, y = Total_Sales, colour=Genre, group=Genre)) + 
  geom_line(size=.2) + 
  geom_point(size=.2)+
  ggtitle('Genre per year') + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45,hjust=1) ,
        plot.title = element_text(hjust=.5)) 

# Most popular game per year
df %>% group_by(Year) %>% 
  arrange(desc(Global_Sales)) %>%
  top_n(1) %>%
  ggplot(aes(x = Name,y = Global_Sales , fill = Year)) + 
  geom_bar(stat = 'identity' , colour='black') + 
  ggtitle('Most popular per year' ) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45,hjust=1),
        plot.title = element_text(hjust=.5)) +
  coord_flip()

# Genre Distribution per year
platform_per_year = df %>% group_by(Platform,Year) %>% summarise(Count = n()) %>% arrange(Year)
ggplot(data = platform_per_year , aes(x = Platform , y = Year)) +
  geom_tile(aes(fill=Count),colour='white') + 
  theme(axis.text.x = element_text(angle=90,hjust=1),
        plot.title = element_text(hjust=.5)) +
  ggtitle('Gener Distribution per year') + 
  scale_fill_gradient(low='white',high='red',breaks=c(50,150,250,350,450),
                      labels=c('Minimum',150,250,350,'Maximum')) +
  scale_x_discrete(expand=c(0,1))+ ## distance away from axes
  scale_y_discrete(expand=c(0,1)) ## distance away from top axes

