# use importlib

import sys
import subprocess
from py4j.java_collections import SetConverter, MapConverter, ListConverter

name = sys.argv[1]
argt = sys.argv[2:]
module = __import__(name)

fx = getattr(module, name)

gateway = JavaGateway(auto_convert=True)


def check(*args):
    py_out = fx(*args)
    p1 = subprocess.Popen(["/usr/bin/java", name], stdout=subprocess.PIPE)
    java_out = p1.stdout.read()
    if py_out == java_out:
        return True
    else:
        print "Python: " + py_out
        print "Java: " + java_out
        return False


check(argt)
