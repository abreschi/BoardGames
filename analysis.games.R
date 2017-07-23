library(data.table)
library(ggplot2)
library(reshape2)

theme_set(theme_bw(base_size=18))


dt = fread("mdata.games.year.tsv")

#------------------
# DATA PROCESSING
#------------------

# Assign playing time to predefined intervals
playingtimes = c(5,10,20,30,40,60,90,120,180,240)
dt[["playingtime"]] = unlist(sapply(replace(dt[["playingtime"]], is.na(dt[["playingtime"]]), 0), function(x) playingtimes[which.min(abs(x-playingtimes))]))

# Assign max number of players to predefined intervals
maxplayers = c(0,1,2,3,4,5,6,7,8,9,10,11,12,16,20,25,100)
dt[["maxplayers"]] = unlist(sapply(replace(dt[["maxplayers"]], is.na(dt[["maxplayers"]]), 0), function(x) maxplayers[which.min(abs(x-maxplayers))]))

# Some year was badly parsed so needs to be converted to numeric
dt[["year"]] = as.numeric(dt[["year"]])
# Replace early years by NA
dt[["year"]] = replace(dt[["year"]], dt[["year"]]<1940 | dt[["year"]]>2020, NA)

# Number of publishers per game
dt[["n_publishers"]] = sapply(dt$boardgamepublisher, function(x)length(strsplit(x, ",")[[1]]))
# Assign max number of publishers to predefined intervals
n_publishers= c(0,1,2,3,4,5,6,7,8,9,10,11,12,16,20,25,100)
dt[["n_publishers"]] = unlist(sapply(replace(dt[["n_publishers"]], is.na(dt[["n_publishers"]]), 0), function(x) n_publishers[which.min(abs(x-n_publishers))]))

# Consider minimum age 18 even for older ages
dt[["minage"]] = replace(dt[["minage"]], dt[["minage"]]>18, 18)

#---------------
# SUMMARY
#---------------

# Cumulative plot of games per year
ggplot(dt[!is.na(year),.N,by="year"][order(year),]) + geom_bar(aes(year, cumsum(N)), stat="identity")


#---------------
# CORRELATIONS
#---------------

# Longer games are perceived as more complicated
ggplot(dt) + geom_density(aes(x=averageweight, color=as.factor(playingtime)), size=3)

# Longer games have higher ratings
ggplot(dt) + geom_boxplot(aes(x=as.factor(playingtime), y=average), size=3)

# Complicated games have higher ratings
ggplot(dt[average>0]) + stat_bin_hex(aes(average, averageweight, fill=log(..count..)), binwidth=0.1)

# Users like more games with 5-10 publishers, no less, no more
ggplot(dt[,mean(average),by="n_publishers"][order(n_publishers),]) + geom_histogram(aes(as.factor(n_publishers),V1), stat="identity")

# Users like more games for teenagers
ggplot(dt[,mean(average),by="minage"]) + geom_bar(aes(x=as.factor(minage),y=V1), stat="identity")

#-------------
# CATEGORIES
#-------------

categories = data.table(melt(strsplit(dt$boardgamecategory, ",")))
categories[["value"]] = factor(categories[["value"]], levels=categories[,.N,by="value"][order(N)][["value"]])
categories[["usersrated"]] = dt[["usersrated"]][match(categories$L1, row.names(dt[,"usersrated"]))]
categories[["average"]] = dt[["average"]][match(categories$L1, row.names(dt[,"average"]))]
categories[["playingtime"]] = dt[["playingtime"]][match(categories$L1, row.names(dt[,"playingtime"]))]
categories[["year"]] = dt[["year"]][match(categories$L1, row.names(dt[,"year"]))]

categories_summary = categories[,list(.N,median(usersrated), mean(average), mean(playingtime)),by="value"]

# Card games and War games are the categories with the most games
ggplot(na.omit(categories)[, .N, by="value"][order(N),]) + geom_histogram(aes(x=value,y=N), stat='identity') + theme(axis.text=element_text(angle=45, hjust=1))

# The most rated categories are related to city building
ggplot(categories_summary) + geom_bar(aes(x=factor(value,levels=categories_summary[order(V2)][["value"]]), y=V2), stat="identity") + theme(axis.text.x=element_text(angle=45,hjust=1))

# Ratings per category
ggplot(categories_summary) + geom_bar(aes(x=factor(value,levels=categories_summary[order(V3)][["value"]]), y=V3), stat="identity") + theme(axis.text.x=element_text(angle=45,hjust=1))

# War and civilization games are longer
ggplot(categories_summary) + geom_bar(aes(x=factor(value,levels=categories_summary[order(V4)][["value"]]), y=V4), stat="identity") + theme(axis.text.x=element_text(angle=45,hjust=1))

# Cluster the categories based on the profile along the year
a = (acast(categories[!is.na(year)&!is.na(value),.N,by=c("year","value")], value~year, value.var="N", fill=0))
pdf("tmp.pdf", h=7, w=15)
plot(hclust(dist(scale(a)), method="complete"))
dev.off()

# Categories have different trends over time
ggplot(categories[!is.na(year),.N,by=c("value","year")][,c("N_avg","N_sd","N_year"):=list(mean(N),sd(N),sum(year)),by="year"][N_year>1000]) + geom_tile(aes(year,value,fill=(N-N_avg)/N_sd))


# Network of categories
categories_matrix = acast(cbind(categories[!is.na(value),], c=1), L1~value, value.var="c", fill=0)


#-------------
# MECHANISMS
#-------------

mechanic = data.table(melt(strsplit(dt[!is.na("boardgamemechanic"),][["boardgamemechanic"]], ",")))
mechanic[["value"]] = factor(mechanic[["value"]], levels=mechanic[,.N,by="value"][order(N)][["value"]])
mechanic[["usersrated"]] = dt[["usersrated"]][match(mechanic$L1, row.names(dt[,"usersrated"]))]
mechanic[["average"]] = dt[["average"]][match(mechanic$L1, row.names(dt[,"average"]))]
mechanic[["playingtime"]] = dt[["playingtime"]][match(mechanic$L1, row.names(dt[,"playingtime"]))]
mechanic[["year"]] = dt[["year"]][match(mechanic$L1, row.names(dt[,"year"]))]

# Levels of mechanic sorted by frequency of playingtime >60min
levMechPlay = names(sort(sweep(table(mechanic$value,mechanic$playingtime>60),1, table(mechanic$value), "/")[,2]))

#categories_summary = categories[,list(.N,median(usersrated), mean(average), mean(playingtime)),by="value"]

# Dice rolling, roll/spin/move and hand management are the most abundant mechanics
ggplot(na.omit(mechanic)[, .N, by="value"][order(N),]) + geom_histogram(aes(x=value,y=N), stat='identity') + theme(axis.text=element_text(angle=45, hjust=1))

# Mechanic have different trends over the year
ggplot(mechanic[!is.na(year)&!is.na(value),.N,by=c("value","year")][,c("N_avg","N_sd","N_year"):=list(mean(N),sd(N),sum(year)),by="year"][N_year>1000]) + geom_tile(aes(as.factor(year),value,fill=(N-N_avg)/N_sd))

# Make a matrix of mechanic vs playing time
mechanic_playingtime = acast(mechanic[,.N,by=c("value","playingtime")], value~playingtime, fill=0, value.var="N")

#--------------
# ARTISTS
#--------------

artists = data.table(melt(strsplit(dt[!is.na(boardgameartist),][["boardgameartist"]], ",")))

# Select only board games where 2 or more artists participated
artists = artists[L1 %in% artists[,.N,by="L1"][N>1][["L1"]],]

#---------------------
# COMMENTS - RATINGS
#---------------------

c = fread("comments.users.country.tsv")

c[["text"]] = NULL
