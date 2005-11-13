#!/usr/bin/python

import sys
import os
import re
import stat

def main():
    topdir = os.path.dirname(sys.argv[0])
    resultdir = os.path.join(topdir,"results")
    testcasedir = os.path.join(topdir,"testcases")
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

                tcfile = open(os.path.join(testcasedir,tcase+".tc"),"r")
                descr = tcfile.readlines()[0]
                tcfile.close()
                
                testcases.append({"name":  tcase,
                                  "utime": utime,
                                  "stime": stime,
                                  "total": (utime+stime),
                                  "descr": descr,
                                  })

    # Print gathered data
    o = sys.stdout
    params = { "pagetitle": "Primenumber Benchmark",
               }
    o.write('''<html>
  <head>
    <title>%(pagetitle)s</title>
    <style>
      table {
        border-collapse: collapse;
      }
      table tr th {
      text-decoration: underline;
      text-align: left;
      }
      table tr td,
      table tr th {
      border: 1px solid black;
      padding: 0px 1ex 0px 1ex;
      }
    </style>
  </head>
  <body>
    <h1>%(pagetitle)s</h1>
''' % (params))
    o.write("""  <p>
    The task solved by these programs is to first find the first 2^16 (65536)
    prime numbers and store them in an array/list and then write a
    list of them into a file. The algorithm used builds a list of
    known prime numbers starting from [2,3] and successively tests
    larger numbers n against this list until the value from the list
    exceeds the square root of n. This algorithm may or may not be the
    most effective one for the respective system.
  </p>\n""")
    o.write('<table>\n  <tr><th>rank</th><th>seconds</th><th>relative factor</th><th>testcase</th><th>description</th></tr>\n')
    testcases.sort(lambda a,b: cmp(a["total"],b["total"]))
    n = 0
    fastest = testcases[0]["total"]
    for tc in testcases:
        n = n + 1
        tc["rank"] = n
        tc["relative"] = tc["total"] / fastest
        o.write("  <tr><td>%(rank)d</td><td>%(total)1.2f</td><td>%(relative)1.2f</td><td>%(name)s</td><td>%(descr)s</td></tr>\n" % tc)
    o.write('</table>\n</body>\n</html>\n')

if __name__ == '__main__':
    main()
