library(data.table)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

theme_set(theme_bw(base_size=18))


dt = fread("mdata.games.year.tsv")

#------------------
# DATA PROCESSING
#------------------

# Assign playing time to predefined intervals
playingtimes = c(0,5,10,20,30,40,60,90,120,180,240)
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
dt[["publishers_bins"]] = cut(dt[["n_publishers"]], breaks=c(0,1,2,3,4,5,10,15,20,Inf))

# Consider minimum age 18 even for older ages
dt[["minage"]] = replace(dt[["minage"]], dt[["minage"]]>18, 18)

# Bin the average rating in 10 bins
dt[["average_bin"]] = cut(dt[["average"]],10)

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

# The most popular games (most owned) have an average rating of 7.5
ggplot(dt[average!=0&owned!=0,]) + geom_hex(aes(average, owned, fill=log10(..count..)))

# Games wiht 20 publishers are the most popular ones (most owned)
ggplot(dt) + geom_boxplot(aes(as.factor(n_publishers), owned))


# Summarize features depending on rankings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p1 = ggplot(dt[average>0,]) 
p1 = p1 + geom_histogram(aes(x=as.factor(average_bin)), stat="count")
p1 = p1 + coord_flip()
p1 = p1 + labs(y="Number of games", x="Average Rating")
p1 = p1 + scale_x_discrete(labels=seq(1,10), expand=c(0.01,0))
p1 = p1 + scale_y_continuous(expand=c(0.001,0))
p1 = p1 + theme(plot.margin = unit(c(1,1,1,1), "lines"))

p2 = ggplot(dt[average>0,])
p2 = p2 + geom_boxplot(aes(x=as.factor(average_bin), y=averageweight, fill=averageweight), notch=T)
p2 = p2 + coord_flip()
p2 = p2 + labs(y="Difficulty", x=NULL)
p2 = p2 + scale_x_discrete(labels=seq(1,10), expand=c(0.01,0))
p2 = p2 + scale_y_continuous(expand=c(0.001,0))
p2 = p2 + theme(
	axis.text.y = element_blank(),
	plot.margin = unit(c(1,1,1,1), "lines"),
)

p3 = ggplot(dt[average>0,])
p3 = p3 + geom_histogram(aes(x=as.factor(average_bin), fill=as.factor(playingtime)), stat="count", position="fill")
p3 = p3 + coord_flip()
p3 = p3 + labs(y="Percentage of games", x=NULL)
p3 = p3 + scale_x_discrete(labels=seq(1,10), expand=c(0.01,0))
p3 = p3 + scale_y_continuous(expand=c(0.001,0), labels=scales::percent)
p3 = p3 + scale_fill_manual(values=rev(brewer.pal(11,"Spectral")), limits=playingtimes[-1], name="Playing\nTime (mins)")
p3 = p3 + theme(legend.position = c(.75, .05), legend.justification = c("left", "bottom"),
	axis.text.y = element_blank(),
	plot.margin = unit(c(1,2,1,1), "lines"),
)

pal4 = colorRampPalette(c("#8B475D", "#DDA0DD", "#98FB98", "#43CD80", "#FFB90F"))(19)
p4 = ggplot(dt[average>0,])
p4 = p4 + geom_histogram(aes(x=factor(average_bin), fill=factor(replace(minage, is.na(minage), 0), levels=rev(c(seq(1:18),0)) )), stat="count", position="fill")
p4 = p4 + coord_flip()
p4 = p4 + labs(y="Percentage of games", x=NULL)
p4 = p4 + scale_x_discrete(labels=seq(1,10), expand=c(0.01,0))
p4 = p4 + scale_y_continuous(expand=c(0.001,0), labels=scales::percent)
p4 = p4 + scale_fill_manual(values=pal4,	limits=seq(1:18), name="Minimum\nAge (yrs)")
p4 = p4 + theme(legend.position = c(.80, .05), legend.justification = c("left", "bottom"),
	axis.text.y = element_blank(),
	plot.margin = unit(c(1,2,1,1), "lines"),
)

pal5 = colorRampPalette(rev(c("#27408B", "#3A5FCD","#4A708B","#87CEFF","#C6E2FF")))(9)
p5 = ggplot(dt[average>0,])
p5 = p5 + geom_histogram(aes(x=factor(average_bin), fill=factor(publishers_bins, levels=rev(c(levels(dt$publishers_bins)[-1],"(0,1]"))) ) , stat="count", position="fill")
p5 = p5 + coord_flip()
p5 = p5 + labs(y="Percentage of games", x=NULL)
p5 = p5 + scale_x_discrete(labels=seq(1,10), expand=c(0.01,0))
p5 = p5 + scale_y_continuous(expand=c(0.001,0), limits=c(0,0.35), labels=scales::percent)
p5 = p5 + scale_fill_manual(values=pal5, limits=levels(dt$publishers_bins)[-1], 
	labels=c(2, 3, 4, 5, "6-10", "11-15","16-20","20+"), name="Publishers")
p5 = p5 + theme(legend.position = c(1.09,0.05), legend.justification = c("right", "bottom"),
	axis.text.y = element_blank(),
	plot.margin = unit(c(1,1,1,1), "lines"),
)

plot1 = plot_grid(p1,p2,p3,p4,p5, nrow=1)
ggsave("plot1.pdf", w=17, h=7)


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
categories_year = acast(categories[!is.na(year)&!is.na(value)&year<=2017,.N,by=c("value","year")], value~year, value.var="N")
norm01 = function(x) {(x-min(x, na.rm=T))/(max(x, na.rm=T)-min(x, na.rm=T))}
categories_year_norm = t(apply(sweep(categories_year,2, colSums(categories_year, na.rm=T), "/"), 1,norm01))

# Categories have different trends over time
lev = names(sort(cutree(hclust(dist(categories_year_norm), "complete"), 10)))
y.labels = do.call(rbind,(strsplit(lev, "_")))[,2]
plot2 = ggplot(melt(categories_year_norm))
plot2 = plot2 + geom_tile(aes(Var2, factor(Var1, levels=lev), fill=value), height=.9)
plot2 = plot2 + scale_fill_gradientn(colors=brewer.pal(10,"YlGnBu"), na.value="white", name="Normalized\nnumber of\ngames per year\n")
plot2 = plot2 + scale_y_discrete(labels=y.labels)
plot2 = plot2 + scale_x_continuous(expand=c(0.01, 0), breaks=seq(1940,2017,10))
plot2 = plot2 + labs(x="Publication Year", y="Game Category")
ggsave("plot2.pdf", w=10, h=15)




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
