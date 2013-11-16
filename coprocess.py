#!/usr/bin/python
import sys
import subprocess
import re
import pipes
tools = set(["mydiff", "emacs"])

while True:
  line = raw_input()
  spl = line.strip().split("\t")
  if len(spl) > 2:
    if spl[1] in tools:
      mount = spl[0]

      # If java file, let's use the real google3 path for eclipse's sake.
      def editFilename(file):
        if not re.match('/google/.+/*.java$', file):
          return mount + file
        return file

      args = [editFilename(a.strip()) for a in spl[2:]]

      shell = False
      if spl[1] == "emacs":
        args.insert(0, "-n")
        args.insert(0, "/usr/local/bin/emacsclient")
      elif spl[1] == "mydiff":
        args.insert(0, "/Users/mtlin/scripts/meldbg")

        args = " ".join(pipes.quote(s) for s in args)
        shell = True
      ret = subprocess.call(args, shell=shell)
      sys.stdout.flush()
