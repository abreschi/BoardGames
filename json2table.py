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
)

print "\t".join(["id"] + [col for col in cols])
for k,v in d.iteritems():
  line = "\t".join(
    [k] +
    [
      ",".join(v.get(col, "NA"))
        if isinstance(v.get(col),list) 
        else str(v.get(col, "NA"))
        for col in cols
    ]
  )
  print line


