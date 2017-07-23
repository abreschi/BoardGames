#!/usr/bin/env python

import json
import sys
reload(sys)
sys.setdefaultencoding('utf8')

f = open(sys.argv[1])
d = json.load(f)
f.close()

cols = (
  "country",
  "firstname",
  "lastlogin",
  "lastname",
  "n_buddies",
  "name",
  "stateorprovince",
  "yearregistered",
)

out1 = open("mdata.users.tsv", "w")
out2 = open("network.users.tsv", "w")

out1.write("\t".join(["id"] + [col for col in cols]) + "\n")
for k,v in d.iteritems():
  line = "\t".join(
    [k] +
    [str(v.get(col, "NA")) 
      if str(v.get(col, "NA"))
      else "NA"
      for col in cols]
  )
  out1.write(line+"\n")

  if int(v["n_buddies"]) > 0:
     buddies = v["buddies"]
     for buddy in buddies:
       out2.write("\t".join((k, buddy[0])) + "\n")
    

out1.close()
out2.close()


