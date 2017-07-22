#!/usr/bin/env python

import urllib
from bs4 import BeautifulSoup
import sys
import json
reload(sys)
sys.setdefaultencoding('utf8')

ID = sys.argv[1]


f = urllib.urlopen("https://www.boardgamegeek.com/xmlapi2/user?name=%s&buddies=1&guilds=1" %(ID))
s = f.read()
f.close()


soup = BeautifulSoup(s)


tag_w_value = (
	"firstname",
	"lastname",
	"yearregistered",
	"lastlogin",
	"stateorprovince",
	"country",
)

users = {}
u = soup.find("user")
users["name"] = u["name"]

for t in tag_w_value:
	users[t] = u.find(t)["value"] if u.find(t) is not None else "NA"
users["n_buddies"] = u.find("buddies")["total"]
for b in u.find_all("buddy"):
	users.setdefault("buddies",[]).append((b["id"], b["name"]))

d = {}
d[u["id"]] = users
print json.dumps(d)

