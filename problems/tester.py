# use importlib

import sys
import subprocess
# from py4j.java_collections import SetConverter, MapConverter, ListConverter

name = sys.argv[1]
str_args = sys.argv[2:]
argt = []
for arg in str_args:
    argt.append(eval(arg))

module = __import__(name)

fx = getattr(module, name)

# gateway = JavaGateway(auto_convert=True)


def check(*args):
    py_out = fx(*args)
    p1 = subprocess.Popen(["/usr/bin/java", "Main", name]+str_args, stdout=subprocess.PIPE)
    java_out = p1.stdout.read()
    if py_out == java_out:
        return True
    else:
        print "Python: " + str(py_out)
        print "Java: " + str(java_out)
        return False

check(*argt)
