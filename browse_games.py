#!/usr/bin/env python

import urllib
from bs4 import BeautifulSoup
import sys

PAGE = sys.argv[1]

f = urllib.urlopen("https://www.boardgamegeek.com/browse/boardgame/page/%s" %(PAGE))
s = f.read()
f.close()



soup = BeautifulSoup(s)



# Extract collection table
table = soup.find("table", attrs={"class":"collection_table"})

# Extract ranking (first column)
rank = [t.get_text().strip() for t in table.find_all("td", attrs={"class":"collection_rank"})]

# Estract figure link
figures = [t.find("img")["src"] if t.find("img") is not None else "N/A"
	for t in table.find_all("td", attrs={"class":"collection_thumbnail"})
]

# 3 values: game ref, game name, year
object_data = [
	(
	t.find("a")["href"],
	t.find("a").get_text(),
	t.find("span").get_text().strip("()") if t.find("span") is not None else "N/A",
	) for t in table.find_all("td", attrs={"class":"collection_objectname"})
]

# Extract geek ratings
geek_ratings = [t.get_text().strip() 
	for i,t in enumerate(table.find_all("td", attrs={"class":"collection_bggrating"})) 
	if i%3==0
]

# Extract average ratings
avg_ratings = [t.get_text().strip() 
	for i,t in enumerate(table.find_all("td", attrs={"class":"collection_bggrating"}))
	if i%3==1
]

# Extract number of voters
num_voters = [t.get_text().strip() 
	for i,t in enumerate(table.find_all("td", attrs={"class":"collection_bggrating"}))
	if i%3==2
]

# Extract shop ref
market = [t.find("a")["href"] for t in table.find_all("td", attrs={"class":"collection_shop"})]


# Print all info as table
for i in range(len(rank)):
	line = u"\t".join((
		rank[i],
		figures[i],
		object_data[i][0], # href
		object_data[i][1], # game name
		object_data[i][2], # year
		geek_ratings[i],
		avg_ratings[i],
		num_voters[i],
		market[i],
	)).encode('utf-8').strip()
	print line
