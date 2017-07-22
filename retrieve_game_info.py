#!/usr/bin/env python

import urllib
from bs4 import BeautifulSoup
import sys
import json

ID = sys.argv[1]


f = urllib.urlopen("https://www.boardgamegeek.com/xmlapi2/thing?id=%s&ratingcomments=1&stats=1" %(ID))
s = f.read()
f.close()


soup = BeautifulSoup(s)


tag_w_value = (
	"yearpublished",
	"minplayers",
	"maxplayers",
	"playingtime",
	"minplaytime",
	"maxplaytime",
	"minage",
	"usersrated",
	"average",
	"bayesaverage",
	"stddev",
	"median",
	"owned",
	"trading",
	"wanting",
	"wishing",
	"numcomments",
	"numweights",
	"averageweight",
)

games = {}
for item in soup.find_all("item"):
	d = {}
	# Extract collection table
	d["description"] = item.find("description").get_text()
	for t in tag_w_value:
		d[t] = item.find(t)["value"]
	
	for l in item.find_all("link"):
		d.setdefault(l["type"], []).append( l["id"] + "_" + l["value"] )
	
	d["n_comments"] = sum(int(c["totalitems"]) for c in item.find_all("comments"))
		
	comments = []
	for i,comment in enumerate(item.find_all("comment")):
		comments.append({})
		for attr in ("username", "rating", "value"):
			comments[i][attr] = comment[attr]
	
	d["comments"] = comments
	
	games[item["id"]] = d

print json.dumps(games)

