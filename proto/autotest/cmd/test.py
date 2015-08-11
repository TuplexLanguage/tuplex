from os import listdir
from os.path import dirname, abspath, isfile, join
from subprocess import call
import sys


def run_python( file ):
    run_cmd( "python " + file )

def run_cmd( cmdline, expected_ret_code=0 ):
    print "\033[90mRunning: '%s'\033[0m" % cmdline
    try:
        retcode = call( cmdline, shell=True )
        if retcode < 0:
            print >>sys.stderr, "\033[0;41mTest failed, child was terminated by signal %s\033[0m" % ( -retcode, )
        else:
            if expected_ret_code == "nonzero":
                if retcode == 0:
                    print >>sys.stderr, "\033[0;41mExpected nonzero return code but was %s\033[0m" % ( retcode, )
            elif retcode != expected_ret_code:
                print >>sys.stderr, "\033[0;41mExpected return code %s but was %s\033[0m" % ( expected_ret_code, retcode )
    except OSError as e:
        print >>sys.stderr, "Execution failed:", e 
        raise


# print options and quit
run_cmd( """txc -help >/dev/null""" )

# invalid options should return 1
run_cmd( """txc -foobar 2>/dev/null""", 1 )

# empty source file
run_cmd( """echo "" | txc -quiet -nojit -nobc""" )

# minimal source file containing error, should return 1
run_cmd( """echo "bad syntax" | txc -quiet -nojit -nobc 2>/dev/null""", 1 )

# run minimal source file (implicit 'return 0')
run_cmd( """echo "main() { }" | txc -quiet -jit -nobc""" )

# run minimal source file with explicit 'return 0'
run_cmd( """echo "main()->Int { return 0; }" | txc -quiet -jit -nobc""" )

# test assertions
run_cmd( """echo "main()->Int { assert TRUE;  return 0; }" | txc -quiet -jit -nobc""" )
run_cmd( """echo "main()->Int { assert FALSE; return 0; }" | txc -quiet -jit -nobc >/dev/null""", "nonzero" )
