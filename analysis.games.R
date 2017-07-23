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

#---------------
# CORRELATIONS
#---------------

# Longer games are perceived as more complicated
ggplot(dt) + geom_density(aes(x=averageweight, color=as.factor(playingtime)), size=3)

# Longer games have higher ratings
ggplot(dt) + geom_boxplot(aes(x=as.factor(playingtime), y=average), size=3)

# Complicated games have higher ratings
ggplot(dt[average>0]) + stat_bin_hex(aes(average, averageweight, fill=log(..count..)), binwidth=0.1)


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

# Network of categories
categories_matrix = acast(cbind(categories[!is.na(value),], c=1), L1~value, value.var="c", fill=0)


#-------------
# MECHANISMS
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



publishers = data.table(melt(strsplit(dt$boardgamepublisher, ",")))

