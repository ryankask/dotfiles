#!/usr/bin/env python
import subprocess
import re
import sys

def make_re(*msgs):
    return re.compile('(%s)' % '|'.join(msgs))

pyflakes_ignore = make_re('unable to detect undefined names')
pyflakes_warning = make_re(
    'imported but unused',
    'is assigned to but never used',
    'redefinition of unused'
)

def run(cmd, args, ignore_re, warning_re):
    try:
        output = subprocess.check_output([cmd, args])
    except subprocess.CalledProcessError as e:
        output = e.output

    for line in output.decode('utf-8').splitlines():
        if not ignore_re.search(line):
            if ': ' in line and warning_re.search(line):
                line = '{}: WARNING {}'.format(*line.split(': ', 1))
            print line

run('pyflakes', sys.argv[1], pyflakes_ignore, pyflakes_warning)
