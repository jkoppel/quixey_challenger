# make classes for extra classes and then just parse them in
# make version for proper programs

import sys
import subprocess
import types

def py_try(name,flag,*args):
    # import from within a folder?
    if flag == True:
        module = __import__("python."+name+"GOOD")
        fx = getattr(module, name+"GOOD")
    else:
        module = __import__("python."+name)
        fx = getattr(module, name)

    try:
        return eval("fx."+name+"(*args)")
    except:
        return sys.exc_info()


def check(name,*args):
    py_out_good = py_try(name,True,*args)
    if isinstance(py_out_good,types.GeneratorType):
        print "Correct: (generator) " + str(list(py_out_good))
    else:
        print "Correct: " + str(py_out_good)

    py_out_test = py_try(name,False,*args)
    if isinstance(py_out_test,types.GeneratorType):
        print "Python: (generator) " + str(list(py_out_test))
    else:
        print "Python: " + str(py_out_test)

    print [str(arg) for arg in args]
    try:
        p1 = subprocess.Popen(["/usr/bin/java", "Main", name]+ \
                            [str(arg) for arg in args], stdout=subprocess.PIPE)
        java_out = p1.stdout.read()
        print "Java: " + str(java_out)
    except:
        print "Java: " + str(sys.exc_info())

# let's do just filename, first line is name of program
# other lines will be input args?
name = sys.argv[1]
working_file = open("testcases/"+name+".txt", 'r')

for line in working_file:
    stuff = eval(line)
    print stuff
    argt, correct = stuff
    if not isinstance(argt, list):
        argt = [argt]

    check(name,*argt)

# str_args = sys.argv[2:]
# argt = []
# for arg in str_args:
#     argt.append(eval(arg))





