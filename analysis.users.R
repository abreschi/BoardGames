library(data.table)
library(igraph)
library(ggplot2)


mdata = fread("mdata.users.tsv", quote="")

# Format dates
mdata[["lastlogin"]] = as.Date(mdata[["lastlogin"]], "%Y-%m-%d")

mdata = mdata[!is.na(id),]


# The number of user registrations increases every year
ggplot(mdata) + geom_histogram(aes(as.factor(yearregistered)), stat="count")

# The users that registered earlier have more buddies
ggplot(mdata[,mean(n_buddies),by="yearregistered"]) + geom_bar(aes(yearregistered,V1), stat="identity")

# Most users are from UK and US
ggplot(mdata[country%in%names(which(table(mdata$country) > 50)),]) + geom_histogram(aes(yearregistered, fill=country), stat="count") + theme(axis.text.x=element_text(angle=45,hjust=1)) + scale_fill_manual(name="", values=c(brewer.pal(12, "Set1"), brewer.pal(12, "Set2")))


# NETWORK

n = fread("network.users.tsv")
n = n[V1!=V2,]
n = data.table(t(apply(n, 1, sort)))
n = n[!duplicated(n),]


g = graph_from_data_frame(n[V1%in%mdata$id & V2%in%mdata$id,], directed=F, vertices=mdata)


V(g)[which(V(g)$country %in% names(which(table(mdata$country) > 50)))]
a = induced_subgraph(g, V(g)[which(V(g)$country %in% names(which(table(mdata$country) > 50)))])
V(a)$color= c(brewer.pal(12,"Paired"), brewer.pal(12,"Set1"))[as.numeric(as.factor(V(a)$country))]

# Color nodes by country
pdf("tmp.pdf")
plot(induced_subgraph(a, V(a)[which(degree(a)>1)]), vertex.size=2, vertex.label=NA)
dev.off()

