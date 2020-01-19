if (!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
}
library("ggplot2")
if (!require(plyr)) {
  install.packages("plyr", repos="http://cran.us.r-project.org")
}
library(plyr)
if (!require(data.table)) {
  install.packages("data.table", repos="http://cran.us.r-project.org")
}
library(data.table)
if (!require(waffle)) {
  install.packages("waffle", repos="http://cran.us.r-project.org")
}
library(waffle)
if (!require(ggthemes)) {
  install.packages("ggthemes", repos="http://cran.us.r-project.org")
}
library(ggthemes)

workingDir = '/Users/michaeltauberg/projects/billboard'

# Popular Music in 2019 - Year in Review
csvName = "billboard_2019b.csv"
data_name = "2019"

setwd(workingDir)

dt = read.csv(csvName)
dt = dt[dt$artist != "",]
#write.csv(dt_uniq, "songs_2019b.csv", row.names = FALSE)
dt = dt[!duplicated(dt[,c('title','artist', "rank", "weeks" )], fromLast=FALSE),] #fromlast to get highest value in "weeks_on_list" field
#dt_uniq = dt_uniq[order(dt_uniq$weeks, decreasing = TRUE),] 
dt = dt[order(dt$date),] 


dt$main_artist = tolower(dt$artist)
dt$main_artist = gsub(" featuring.*", "", dt$main_artist)
dt$main_artist = gsub(" with .*", "", dt$main_artist)
dt$main_artist = gsub(" & .*", "", dt$main_artist)
#dt$main_artist = gsub(" and .*", "", dt$main_artist)
dt$main_artist = gsub(" x .*", "", dt$main_artist)
dt$main_artist = gsub(", .*", "", dt$main_artist)
dt$main_artist = gsub(" duet.*", "", dt$main_artist)
dt$main_artist = gsub(" co-starring.*", "", dt$main_artist)
dt$main_artist = gsub("travi$", "travis", dt$main_artist)
dt$main_artist = gsub("jay z", "jay-z", dt$main_artist)
dt$main_artist = gsub("\\\"misdemeanor\\\"", "misdemeanor", dt$main_artist)
dt$main_artist = gsub(" + .*", "", dt$main_artist)
dt$main_artist = gsub("jay-z +.*", "", dt$main_artist)
dt$main_artist = gsub(" vs.*", "", dt$main_artist)
dt$main_artist = factor(dt$main_artist)
dt$long_title = paste(toupper(dt$main_artist), dt$title, sep=" - ")

# count the number of artist rows
artists  = dt %>% group_by(main_artist) %>% summarize(no_rows = count(main_artist))
#artists = ddply(dt, "main_artist", summarise,
#      no_rows = length(main_artist))
artists = artists$no_rows
colnames(artists) = c("main_artist", "no_rows")
artists = artists[order(artists$no_rows, decreasing = TRUE),] 
top_artists = artists[0:20,]



############
#Hit Songs
###########

hits = dt[dt$rank == 1,]
ones  = hits %>% group_by(main_artist) %>% summarize(no_rows = count(main_artist))

data_name = "hits"
hit_artists = ones$no_rows
colnames(hit_artists) = c("main_artist", "no_rows")
hit_artists = hit_artists[order(hit_artists$no_rows, decreasing = TRUE),] 
data_name = "hit_artists"
hit_artists$main_artist = factor(hit_artists$main_artist, levels = hit_artists$main_artist[order(hit_artists$no_rows, decreasing=FALSE)])
p = ggplot(hit_artists, aes(x=main_artist, y=no_rows)) + geom_bar(stat="identity") 
p = p + ggtitle("Number of Weeks at No 1 of the Billboard Hot-100 in 2019")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=14), axis.title=element_text(size=18,face="bold"))
p = p + xlab("Artist") + ylab("Weeks at No 1 on Billboard Hot-100 in 2019") 
p = p + coord_flip()
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=10, height=7)




###########################
#### ALL ARTISTS
##########################
all_artists = read.csv("all_artists_2019b.csv")

# weeks per genre
#weeks_genre  = all_artists %>% group_by(genre) %>% summarize(no_rows = sum(no_rows))
#weeks_genre = weeks_genre$no_rows
#colnames(weeks_genre) = c("genre", "no_rows")
#weeks_genre = weeks_genre[weeks_genre$genre != "",]

weeks_genre = ddply(all_artists, "genre", summarise, no_rows = sum(no_rows))

data_name = "weeks_genre"
weeks_genre$genre = factor(weeks_genre$genre, levels = weeks_genre$genre[order(weeks_genre$no_rows, decreasing=FALSE)])
p = ggplot(weeks_genre, aes(x=genre, y=no_rows, fill="green")) + geom_bar(stat="identity") 
p = p + ggtitle("Genre by weeks on the chart in 2019")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Genre") + ylab("Weeks on Billboard Hot-100 in 2019") 
p = p + coord_flip()
p = p + guides(fill=FALSE)
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=9, height=7)

# Gender
genders = c()
total = nrow(all_artists)
for (gender in levels(droplevels(all_artists$gender))) {
  gender_subset = all_artists[all_artists$gender==gender,]
  gender_total = nrow(gender_subset)
  stats_row = c(gender,gender_total,total,gender_total/total*100 )
  genders = rbind(genders, stats_row)
}
genders = as.data.table(genders)
genders = as.data.frame(genders)
colnames(genders) = c("genders","num_genders","total","percent_genders")
genders$percent_genders=as.numeric(as.character(genders$percent_genders))
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    axis.text.x=element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
p = ggplot(genders, aes(x="", y=percent_genders, fill=genders)) + geom_bar(width = 1, stat = "identity")
p = p + coord_polar("y", start=0) 
p = p + blank_theme 
p = p + ggtitle("Gender of Billboard Hot-100 Artists in 2019")
p = p + scale_fill_manual(values=c("pink","light blue","grey"))
ggsave(filename = "./genders_2019.png", plot=p, width=6, height=6) 

vals = as.numeric(as.character(genders$num_genders))
val_names  =  sprintf("%s (%s)", c("Female", "Male", "Mixed"), scales::percent(round(vals/sum(vals), 2)))
names(vals)  =  val_names

waffle::waffle(vals,
               colors = c("pink","light blue","grey"), 
               title = "Top 2019 Billboard Artists by Gender")
  ggthemes::scale_fill_tableau(name=NULL)

####################
###"top_artists"
##################
#genre
data_name = "top_artists_genre"
top_artists = all_artists[0:20,]
top_artists = top_artists[order(top_artists$no_rows, decreasing=FALSE),]
top_artists$main_artist = factor(top_artists$main_artist, levels = top_artists$main_artist[order(top_artists$no_rows, decreasing=FALSE)])
p = ggplot(top_artists, aes(x=main_artist, y=no_rows, fill=genre)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 Billboard Hot-100 Artists by weeks on the chart in 2019")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=14), axis.title=element_text(size=18,face="bold"))
p = p + xlab("Artist") + ylab("Weeks on Billboard Hot-100 in 2019") 
p = p + coord_flip()
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=10, height=7)

#gender
data_name = "top_artists_gender"
top_artists = top_artists[order(top_artists$no_rows, decreasing=TRUE),]
top_artists$main_artist = factor(top_artists$main_artist, levels = top_artists$main_artist[order(top_artists$no_rows, decreasing=TRUE)])
p = ggplot(top_artists, aes(x=main_artist, y=no_rows, fill=gender)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 Billboard Hot-100 Artists by weeks on the chart in 2019")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Artist") + ylab("Weeks on Billboard Hot-100 in 2019") 
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=9, height=7)

#age
data_name = "top_artists_age"
km <- kmeans(top_artists$age,centers=3)
top_artists$cluster <- as.factor(km$cluster)
p = ggplot(top_artists, aes(x=age, fill=cluster)) + geom_histogram(binwidth=1)
p = p + ggtitle("Age of Top 20 Artists in 2019")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p  + ylab("num artists in top 20") 
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=9, height=7)

######
# SONGS
########
songs  = dt %>% group_by(long_title) %>% summarize(no_rows = count(long_title))
songs = songs$no_rows
colnames(songs) = c("title", "no_rows")
songs = songs[songs$title != "",]
songs = songs[order(songs$no_rows, decreasing = TRUE),] 
top_songs = songs[0:20,]
data_name = "top_songs"
top_songs = top_songs[order(top_songs$no_rows, decreasing=FALSE),]
top_songs$title = factor(top_songs$title, levels = top_songs$title[order(top_songs$no_rows, decreasing=FALSE)])
p = ggplot(top_songs, aes(x=title, y=no_rows, fill=genre)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 Billboard Hot-100 Artists by weeks on the chart in 2019")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Song") + ylab("Weeks on Billboard Hot-100 in 2019") 
p = p + coord_flip()
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=10, height=6)


####
# HIT SONGS - number ones
####
hits = dt[dt$rank == 1,]
num_ones = hits %>% 
  group_by(long_title,artist) %>%
  summarise(no_rows = count(long_title))
num_ones = num_ones$no_rows
colnames(num_ones) = c("title", "no_rows")
data_name = "hit_songs"
num_ones$title = factor(num_ones$title, levels = num_ones$title[order(num_ones$no_rows, decreasing=FALSE)])
p = ggplot(num_ones, aes(x=title, y=no_rows)) + geom_bar(stat="identity") 
p = p + ggtitle("#1 Hits Songs in 2019")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Song") + ylab("Weeks at #1") 
p = p + coord_flip()
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=7, height=10)


########
### GENRES ###
########
genres = c()
total = nrow(all_artists)
for (genre in levels(droplevels(all_artists$genre))) {
  genre_subset = all_artists[all_artists$genre==genre,]
  genre_total = nrow(genre_subset)
  stats_row = c(genre,genre_total,total,genre_total/total*100 )
  genres = rbind(genres, stats_row)
}
genres = as.data.table(genres)
genres = as.data.frame(genres)
colnames(genres) = c("genres","num_genres","total","percent_genres")
genres$percent_genres=as.numeric(as.character(genres$percent_genres))

vals = as.numeric(as.character(genres$num_genres))
val_names  =  sprintf("%s (%s)", c("country", "edm", "pop", "r&b", "rap", "reggaeton", "rock"), scales::percent(round(vals/sum(vals), 2)))
names(vals)  =  val_names

waffle::waffle(vals,
               size = 1, 
               #colors = c("pink","light blue","grey"), 
               title = "Billboard 2019 Artists by Genre")
ggthemes::scale_fill_tableau(name=NULL)
png('genre_waffle.png')
dev.off()




# age of artists by genre
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# plot bar chart of ages by genre
ages = c()
all_artists = all_artists[!is.na(all_artists$genre),]
all_artists = all_artists[all_artists$genre != "",]
all_artists$age = as.numeric(as.character(all_artists$age))
for (genre in levels(droplevels(all_artists$genre))) {
  print(genre)
  genre_subset = all_artists[all_artists$genre==genre,]
  median_age = median(genre_subset$age, na.rm = TRUE)
  mean_age = mean(genre_subset$age, na.rm = TRUE)
  stats_row = c(genre,median_age,mean_age)
  ages = rbind(ages, stats_row)
}
# plot the histogram of ages
reggaeton = all_artists[all_artists$genre=="reggaeton",]
p = ggplot(reggaeton, aes(x=age, fill="red")) + geom_histogram(binwidth=1)
p = p + ggtitle("Age of Reggaeton Artists in 2019")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=10), axis.title=element_text(size=10))
p = p  + ylab("num reggaeton artists") + guides(fill=FALSE)
p1 = p
country = all_artists[all_artists$genre=="country",]
p = ggplot(country, aes(x=age, fill="red")) + geom_histogram(binwidth=1)
p = p + ggtitle("Age of Country Artists in 2019")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=10), axis.title=element_text(size=10))
p = p  + ylab("num country artists") + guides(fill=FALSE)
p2 = p
pop = all_artists[all_artists$genre=="pop",]
p = ggplot(pop, aes(x=age, fill="red")) + geom_histogram(binwidth=1)
p = p + ggtitle("Age of Pop Artists in 2019")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=10), axis.title=element_text(size=10))
p = p  + ylab("num pop artists") + guides(fill=FALSE)
p3 = p
rb = all_artists[all_artists$genre=="r&b",]
p = ggplot(rb, aes(x=age, fill="red")) + geom_histogram(binwidth=1)
p = p + ggtitle("Age of R&B Artists in 2019")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=10), axis.title=element_text(size=10))
p = p  + ylab("num r&b artists") + guides(fill=FALSE)
p4 = p
rap = all_artists[all_artists$genre=="rap",]
p = ggplot(rap, aes(x=age, fill="red")) + geom_histogram(binwidth=1)
p = p + ggtitle("Age of Rap Artists in 2019")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=10), axis.title=element_text(size=10))
p = p + ylab("num rap artists") + guides(fill=FALSE)
p5 = p
edm = all_artists[all_artists$genre=="edm",]
p = ggplot(edm, aes(x=age, fill="red")) + geom_histogram(binwidth=1)
p = p + ggtitle("Age of EDM Artists in 2019")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=10), axis.title=element_text(size=10))
p = p + ylab("num edm artists") + guides(fill=FALSE)
p6 = p

png("./age_multiplot.png", width=700, height=700)
p = multiplot(p1, p2, p3, p4, p5, p6, cols=2)
dev.off()

######
### Gender by Genre
#######
# plot bar chart of ages by genre
all_artists = all_artists[!is.na(all_artists$gender),]
all_artists = all_artists[all_artists$gender != "",]
# plot the histogram of ages
reggaeton = all_artists[all_artists$genre=="reggaeton",]
genders = c()
total = nrow(reggaeton)
for (gender in levels(droplevels(reggaeton$gender))) {
  gender_subset = reggaeton[reggaeton$gender==gender,]
  gender_total = nrow(gender_subset)
  stats_row = c(gender,gender_total,total,gender_total/total*100 )
  genders = rbind(genders, stats_row)
}
genders = as.data.table(genders)
genders = as.data.frame(genders)
colnames(genders) = c("genders","num_genders","total","percent_genders")
genders$percent_genders=as.numeric(as.character(genders$percent_genders))

p = ggplot(genders, aes(x="", y=percent_genders, fill=genders)) + geom_bar(width = 1, stat = "identity")
p = p + coord_polar("y", start=0) 
p = p + blank_theme 
p = p + ggtitle("Gender of Hot-100 Reggaeton Artists in 2019")
p = p + scale_fill_manual(values=c("light blue","grey", "pink"))
p1 = p

country = all_artists[all_artists$genre=="country",]
genders = c()
total = nrow(country)
for (gender in levels(droplevels(country$gender))) {
  gender_subset = country[country$gender==gender,]
  gender_total = nrow(gender_subset)
  stats_row = c(gender,gender_total,total,gender_total/total*100 )
  genders = rbind(genders, stats_row)
}
genders = as.data.table(genders)
genders = as.data.frame(genders)
colnames(genders) = c("genders","num_genders","total","percent_genders")
genders$percent_genders=as.numeric(as.character(genders$percent_genders))

p = ggplot(genders, aes(x="", y=percent_genders, fill=genders)) + geom_bar(width = 1, stat = "identity")
p = p + coord_polar("y", start=0) 
p = p + blank_theme 
p = p + ggtitle("Gender of Hot-100 Country Artists in 2019")
p = p + scale_fill_manual(values=c("pink","light blue", "grey"))
p2 = p

pop = all_artists[all_artists$genre=="pop",]
genders = c()
total = nrow(pop)
for (gender in levels(droplevels(pop$gender))) {
  gender_subset = pop[pop$gender==gender,]
  gender_total = nrow(gender_subset)
  stats_row = c(gender,gender_total,total,gender_total/total*100 )
  genders = rbind(genders, stats_row)
}
genders = as.data.table(genders)
genders = as.data.frame(genders)
colnames(genders) = c("genders","num_genders","total","percent_genders")
genders$percent_genders=as.numeric(as.character(genders$percent_genders))

p = ggplot(genders, aes(x="", y=percent_genders, fill=genders)) + geom_bar(width = 1, stat = "identity")
p = p + coord_polar("y", start=0) 
p = p + blank_theme 
p = p + ggtitle("Gender of Hot-100 Pop Artists in 2019")
p = p + scale_fill_manual(values=c("pink","light blue", "grey"))
p3 = p

rb = all_artists[all_artists$genre=="r&b",]
genders = c()
total = nrow(rb)
for (gender in levels(droplevels(rb$gender))) {
  gender_subset = rb[rb$gender==gender,]
  gender_total = nrow(gender_subset)
  stats_row = c(gender,gender_total,total,gender_total/total*100 )
  genders = rbind(genders, stats_row)
}
genders = as.data.table(genders)
genders = as.data.frame(genders)
colnames(genders) = c("genders","num_genders","total","percent_genders")
genders$percent_genders=as.numeric(as.character(genders$percent_genders))

p = ggplot(genders, aes(x="", y=percent_genders, fill=genders)) + geom_bar(width = 1, stat = "identity")
p = p + coord_polar("y", start=0) 
p = p + blank_theme 
p = p + ggtitle("Gender of Hot-100 R&B Artists in 2019")
p = p + scale_fill_manual(values=c("pink","light blue", "grey"))
p4 = p

rap = all_artists[all_artists$genre=="rap",]
genders = c()
total = nrow(rap)
for (gender in levels(droplevels(rap$gender))) {
  gender_subset = rap[rap$gender==gender,]
  gender_total = nrow(gender_subset)
  stats_row = c(gender,gender_total,total,gender_total/total*100 )
  genders = rbind(genders, stats_row)
}
genders = as.data.table(genders)
genders = as.data.frame(genders)
colnames(genders) = c("genders","num_genders","total","percent_genders")
genders$percent_genders=as.numeric(as.character(genders$percent_genders))

p = ggplot(genders, aes(x="", y=percent_genders, fill=genders)) + geom_bar(width = 1, stat = "identity")
p = p + coord_polar("y", start=0) 
p = p + blank_theme 
p = p + ggtitle("Gender of Hot-100 Rap Artists in 2019")
p = p + scale_fill_manual(values=c("pink","light blue", "grey"))
p5 = p

edm = all_artists[all_artists$genre=="edm",]
genders = c()
total = nrow(edm)
for (gender in levels(droplevels(edm$gender))) {
  gender_subset = edm[edm$gender==gender,]
  gender_total = nrow(gender_subset)
  stats_row = c(gender,gender_total,total,gender_total/total*100 )
  genders = rbind(genders, stats_row)
}
genders = as.data.table(genders)
genders = as.data.frame(genders)
colnames(genders) = c("genders","num_genders","total","percent_genders")
genders$percent_genders=as.numeric(as.character(genders$percent_genders))

p = ggplot(genders, aes(x="", y=percent_genders, fill=genders)) + geom_bar(width = 1, stat = "identity")
p = p + coord_polar("y", start=0) 
p = p + blank_theme 
p = p + ggtitle("Gender of Hot-100 EDM Artists in 2019")
p = p + scale_fill_manual(values=c("light blue", "grey", "pink"))
p6 = p

png("./gender_multiplot.png", width=700, height=700)
p = multiplot(p1, p2, p3, p4, p5, p6, cols=2)
dev.off()



############
#Histogram of shortest band names
###########

# 3 chris

all_artists$letter = factor(substr(all_artists$main_artist, 1, 1))

data_name = "artist_letter"
letters = c()
total = nrow(all_artists)
for (letter in levels(droplevels(all_artists$letter))) {
  letter_subset = all_artists[all_artists$letter==letter,]
  letter_total = nrow(letter_subset)
  stats_row = c(letter,letter_total,total,letter_total/total*100 )
  letters = rbind(letters, stats_row)
}
letters = as.data.table(letters)
letters = as.data.frame(letters)
colnames(letters) = c("letter","num_letters","total","percent_letters")
letters$percent_letters=as.numeric(as.character(letters$percent_letters))

data_name = "artist_letters_bar"
letters$num_letters = as.numeric(as.character(letters$num_letters))
letters = letters[order(letters$num_letters, decreasing=TRUE),]
letters$letter = factor(letters$letter, levels = letters$letter[order(letters$num_letters, decreasing=TRUE)])
p = ggplot(letters, aes(x=letter, y=num_letters)) + geom_bar(stat="identity") 
p = p + xlab("Letter") + ylab("Number of Hot-100 Artists that Start with Letter") 
p = p + ggtitle("Number of Artists that Start with Letter")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + guides(fill=FALSE)
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=9, height=5)

l = all_artists[all_artists$letter == 'l',]  # 6 lils
c = all_artists[all_artists$letter == 'c',]  # 5 chris'


# Nationality
# Gender
countries = c()
total = nrow(all_artists)
for (nationality in levels(droplevels(all_artists$nationality))) {
  country_subset = all_artists[all_artists$nationality==nationality,]
  countryr_total = nrow(country_subset)
  stats_row = c(nationality,countryr_total,total,countryr_total/total*100 )
  countries = rbind(countries, stats_row)
}
countries = as.data.table(countries)
countries = as.data.frame(countries)
colnames(countries) = c("countries","num_countries","total","percent_countries")
countries$percent_countries=as.numeric(as.character(countries$percent_countries))

data_name = "countries"
countries = countries[order(countries$percent_countries, decreasing=FALSE),]
countries$countries = factor(countries$countries, levels = countries$countries[order(countries$percent_countries, decreasing=FALSE)])
p = ggplot(countries, aes(x=countries, y=percent_countries)) + geom_bar(stat="identity") 
p = p + xlab("Artist Nationality") + ylab("Percentage") 
p = p + ggtitle("Natiobality of Hot-100 Artists")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + guides(fill=FALSE)
p = p + coord_flip()
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=9, height=5)
