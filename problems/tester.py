use importlib

import sys

mm = sys.argv[1]


import foo
fx = getattr(foo, 'bar')()


def install():
    print "In install"

    method_name = 'install' # set by the command line options
    possibles = globals().copy()
    possibles.update(locals())
    method = possibles.get(method_name)()
    if not method:
        raise Exception("Method %s not implemented" % method_name)
    method()


subprocess.check_output
Popen.communicate
pexpect

p1 = subprocess.Popen(["/usr/bin/java", "MyClass"], stdout=subprocess.PIPE)
print p1.stdout.read()

Use Popen.communicate() to perform a blocking read until popened process terminates
