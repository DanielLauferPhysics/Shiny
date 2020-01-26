library(tidyverse)
#library(reshape2)
library(data.table)
library(leaflet)
library(htmltools)
#library(DT)

airbnb = fread(file='new-york-city-airbnb-open-data/AB_NYC_2019.csv', stringsAsFactors = FALSE)

#cast NA's in reviews_per_month column as zeros
airbnb$reviews_per_month[is.na(airbnb$reviews_per_month)] <- 0

#remove rows with zero price
airbnb <- airbnb %>% filter(., price > 0)

#remove rows associated with inactive listing at the time of data collection
airbnb <- airbnb %>% filter(., availability_365 > 0)

#filter out listings with no titles and assign the character count of the remaining to name.length
airbnb = filter(airbnb, name!='') 
airbnb['name.length'] = nchar(airbnb$name)
View(airbnb)

# create variable with colnames as choice
choice <- colnames(airbnb)[-1]

#Correlation matrices are symmetric, so I'm removing the upper half of the entries

get_tri <- function(corrmatrix){
  corrmatrix[lower.tri(corrmatrix)] <- NA
  return(corrmatrix)
}

#helper function orders the correlation matrix by ascending order

reorder_cordata <- function(corrmatrix){
  dmatrix <- as.dist((1-corrmatrix)/2)
  hc <- hclust(dmatrix)
  corrmatrix <- corrmatrix[hc$order, hc$order]
}

#compute the correlation matrix and keep non-redundant information
numericdata <- select(airbnb, colnames(airbnb)[sapply(airbnb, is.numeric)])
View(numericdata)
cordata <- numericdata %>% cor() %>% round(., 2)
cordata <- reorder_cordata(cordata)
cordata <- get_tri(cordata)

#melt the data into a format that can be read into ggplot
melted_cordata <- melt(cordata)

ggplot(data = melted_cordata, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
    midpoint = 0, limit = c(-1,1), space = "Lab", 
    name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
      size = 12, hjust = 1))+
    coord_fixed()

ggheatmap <- ggplot(melted_cordata, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

#distribution of how many listings are owned by a single proprietor
listings_sum <- airbnb %>% 
  group_by(.,host_id, neighbourhood_group) %>% 
  summarise(., tot=log(n(), 2)) %>% 
  filter(., tot>0)

list_above_5 <- airbnb %>% 
  group_by(., host_id,neighbourhood_group) %>% 
  summarise(., tot=log(n(),2)) %>% 
  filter(., (tot>5))

#ask about cleaning up box and whiskers plot
ggplot(data=listings_sum, aes(y=tot)) + geom_boxplot(size=0.05) + facet_wrap(~neighbourhood_group, scales='free') + labs(title='Stuff', x='Number of listings', y='Owner ID')
ggplot(data=list_above_5, aes(y=tot)) + geom_boxplot(size=0.05) + facet_wrap(~neighbourhood_group, scales='free') + labs(title='Stuff', x='Number of listings', y='Owner ID')
summary(listings_sum)

#length of the listing name and its relationship to conversion rate
name.lengths = airbnb %>%  group_by(., neighbourhood_group, name)
ggplot(data=name.lengths, aes(x=name.length)) + geom_density(aes(color=neighbourhood_group)) 

#room type and pricing (clean up point size, highlight high number of anomolous prices by category)

room_dat = airbnb %>% 
  group_by(., neighbourhood_group, room_type, price) 
room_dat$price <- log(room_dat$price, 2)

#this plot is ready for finalization
ggplot(data = room_dat, aes(x=room_type, y=price)) + geom_boxplot() + facet_wrap(~neighbourhood_group, ncol = 2)

#availability during the year as related to min nights and price

avail_data = airbnb %>% group_by(., neighbourhood_group, availability_365, minimum_nights)

ggplot(data = avail_data, aes(y=availability_365, x=neighbourhood_group)) + geom_boxplot()
ggplot(data=avail_data, aes(x=availability_365)) + geom_density(aes(color=neighbourhood_group))

#ready for finalizing
ggplot(data = avail_data, aes(y=log(minimum_nights,2), x=neighbourhood_group)) + geom_boxplot()

ggplot(data=avail_data, aes(x=log(minimum_nights,2))) + geom_density(aes(color=neighbourhood_group)) + coord_cartesian(xlim=c(0,8)) + theme()

#distribution of listings by neighourhood and borough and their pricing

neighbor_data <- airbnb %>% group_by(., neighbourhood, neighbourhood_group) %>% summarise(., tot = n())

#helper function to label points
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

#sample stuff to label outliers with neighbourhood names
dat <- mtcars %>% tibble::rownames_to_column(var="outlier") %>% group_by(cyl) %>%
              mutate(is_outlier=ifelse(is_outlier(drat), drat, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)

ggplot(dat, aes(y=drat, x=factor(cyl))) + geom_boxplot() + geom_text(aes(label=outlier),na.rm=TRUE,nudge_y=0.05)

#worthy of finalizing
ggplot(data=neighbor_data, aes(y=tot))  + geom_boxplot() + facet_wrap(~neighbourhood_group, scale='free') 

#worthy of finalizing
ggplot(data=neighbor_data, aes(x=reorder(neighbourhood,tot), y=tot))  + geom_col() + facet_wrap(~neighbourhood_group, scale='free') + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + coord_flip()
  
#considering the attributes of the top 10% of listings by price

pricey_data <- airbnb %>% group_by(., price, neighbourhood, neighbourhood_group) %>% arrange(., desc(price)) %>% summarise(., tot=n())
View(pricey_data)
nrow(pricey_data)
ggplot(data=pricey_data, aes(x=neighbourhood, y=price)) + geom_boxplot(size=0.5) + facet_wrap(~neighbourhood_group, scale='free') + coord_flip()
ggplot(data=pricey_data, aes(x=price)) + geom_density(aes(color=neighbourhood_group)) + coord_cartesian(xlim=c(0,1200))

airbnb %>% 
  group_by(., neighbourhood) %>% 
  summarise(., avg_lat=mean(latitude), avg_long=mean(longitude)) %>% 
  select(., avg_lat, avg_long) %>% 
  leaflet(data=.) %>%
  addProviderTiles(providers$CartoDB.Positron)

View(mean_pos)



