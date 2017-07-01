# make classes for extra classes and then just parse them in
# make version for proper programs

import copy
import json
import sys
import subprocess
import types

def py_try(algo,flag,*args):
    if flag:
        module = __import__("python."+algo+"GOOD")
        fx = getattr(module, algo+"GOOD")
    else:
        module = __import__("python."+algo)
        fx = getattr(module, algo)

    try:
        return getattr(fx,algo)(*args)
    except:
        return sys.exc_info()


def check(algo,*args):
    args1 = copy.deepcopy(args)
    args2 = copy.deepcopy(args)
    py_out_good = py_try(algo,True,*args1)
    # print "after: "+str(args)
    if isinstance(py_out_good,types.GeneratorType):
        print("Correct: (generator) " + str(list(py_out_good)))
    else:
        print("Correct: " + str(py_out_good))

    py_out_test = py_try(algo,False,*args2)
    # print "after: "+str(args)
    if isinstance(py_out_test,types.GeneratorType):
        print("Python: (generator) " + str(list(py_out_test)))
    else:
        print("Python: " + str(py_out_test))

    try:
        p1 = subprocess.Popen(["/usr/bin/java", "Main", algo]+ \
                            [str(arg) for arg in args], stdout=subprocess.PIPE)
        java_out = p1.stdout.read()
        print("Java: " + str(java_out))
    except:
        print("Java: " + str(sys.exc_info()))


if __name__ == "__main__":
    algo = sys.argv[1]
    working_file = open("json_testcases/"+algo+".json", 'r')

    for line in working_file:
        py_testcase = json.loads(line)
        print(py_testcase)
        test_in, test_out = py_testcase

        check(algo, test_in)






