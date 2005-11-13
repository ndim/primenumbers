#!/usr/bin/python

import sys
import os
import re
import stat

def main():
    resultdir = sys.argv[1]
    # Read files
    usertime   = re.compile("\s+User time \(seconds\):\s+(\S+)\s*")
    systemtime = re.compile("\s+System time \(seconds\):\s+(\S+)\s*")
    testcases = []
    for root, dirs, files in os.walk(resultdir):
        for fname in files:
            basename = os.path.basename(fname)
            (tcase,ext) = os.path.splitext(basename)
            if ext != '.time':
                continue

            utime = None
            stime = None
            timefname = os.path.join(root,tcase+'.time')
            statdata = os.stat(timefname)
            if statdata[stat.ST_SIZE]:
                timefile = open(timefname, "r")
                for line in timefile.readlines():
                    sm = systemtime.match(line)
                    um = usertime.match(line)
                    if sm:
                        stime = float(sm.group(1))
                    elif um:
                        utime = float(um.group(1))
                timefile.close()
            
                testcases.append({"name":tcase,
                                  "utime":utime, "stime":stime,
                                  "total":(utime+stime)})

    # Print gathered data
    o = sys.stdout
    o.write('<html>\n<head>\n<title>Primenumber Benchmark</title>\n</head>\n')
    o.write('<body>\n')
    o.write("""  <p>
    The task solved by these programs is to first find the first 2^16 (65536)
    prime numbers and store them in an array/list and then write a
    list of them into a file. The algorithm used builds a list of
    known prime numbers starting from [2,3] and successively tests
    larger numbers n against this list until the value from the list
    exceeds the square root of n. This algorithm may or may not be the
    most effective one for the respective system.
  </p>\n""")
    o.write('<ol>\n')
    testcases.sort(lambda a,b: cmp(a["total"],b["total"]))
    for tc in testcases:
        o.write("  <li>%(total)1.2f %(name)s</li>\n" % tc)
    o.write('</ol>\n</body>\n</html>\n')

if __name__ == '__main__':
    main()
