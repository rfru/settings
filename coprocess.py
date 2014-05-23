#!/usr/bin/python
import sys
import subprocess
import re
import pipes
tools = set(["ksdiff", "lmeld"])

while True:
  line = raw_input()
  spl = line.strip().split("\t")
  if len(spl) > 2:
    if spl[1] in tools:
      mount = spl[0]

      def parseArg(a):
        if not re.match('^--.*$', a):
          return mount + a
        return a
      args = [parseArg(a.strip()) for a in spl[2:]]

      shell = False
      if spl[1] == "ksdiff":
        args.insert(0, "/usr/local/bin/ksdiff")
      elif spl[1] == "lmeld":
        print args
        args.insert(0, "/Users/mtlin/scripts/meldbg")

        # args = " ".join(pipes.quote(s) for s in args)
        shell = True
      else:
        continue
      ret = subprocess.call(args, shell=shell)
      sys.stdout.flush()
