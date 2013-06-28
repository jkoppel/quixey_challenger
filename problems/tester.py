# make classes for extra classes and then just parse them in
# make version for proper programs

import sys
import subprocess

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
    py_out_test = py_try(name,False,*args)
    p1 = subprocess.Popen(["/usr/bin/java", "Main", name]+ \
                          [str(arg) for arg in argt], stdout=subprocess.PIPE)
    java_out = p1.stdout.read()


    print "Correct: " + str(py_out_good)
    print "Python: " + str(py_out_test)
    print "Java: " + str(java_out)

# let's do just filename, first line is name of program
# other lines will be input args?
name = sys.argv[1]
working_file = open("testcases/"+name+".txt", 'r')

for line in working_file:
    stuff = eval(line)
    print stuff
    argt, correct = stuff

    check(name,*argt)

# str_args = sys.argv[2:]
# argt = []
# for arg in str_args:
#     argt.append(eval(arg))





