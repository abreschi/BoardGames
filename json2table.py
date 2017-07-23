#!/usr/bin/env python

import json
import sys
reload(sys)
sys.setdefaultencoding('utf8')

f = open(sys.argv[1])
d = json.load(f)
f.close()

cols = (
  "average",
  "boardgameartist",
  "boardgamecategory",
  "boardgamedesigner",
  "boardgamefamily",
  "boardgamemechanic",
  "boardgamepublisher",
  "maxplayers",
  "maxplaytime",
  "minage",
  "minplayers",
  "minplaytime",
  "n_comments",
  "numcomments",
  "owned",
  "playingtime",
  "usersrated",
  "wanting",
  "trading",
  "wishing",
  "numweights",
  "averageweight",
)

out1 = open("mdata.games.tsv", "w")
out3 = open("comments.users.tsv", "w")

out1.write("\t".join(["id"] + [col for col in cols]) + "\n")
out3.write("\t".join(("id", "rating", "username", "text")) + "\n")

for k,v in d.iteritems():

  line = "\t".join(
    [k] +
    [
      ",".join(v.get(col, "NA")).replace("\t", " ")
        if isinstance(v.get(col),list) 
        else str(v.get(col, "NA"))
        for col in cols
    ]
  )
  out1.write(line + "\n")

  comments = v.get("comments", [])
  for comment in comments:
    out3.write("\t".join((
      k, 
      comment["rating"], 
      comment["username"],
      comment["value"].replace("\n"," ").replace("\t", " ") 
        if comment["value"]
        else "NA",
    ))
    + "\n")


out1.close()
out3.close()


