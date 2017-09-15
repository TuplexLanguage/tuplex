from os import listdir, chdir
from os.path import dirname, abspath, isfile, join
from subprocess import call
import sys


def run_python( file ):
    run_cmd( "python " + file )

def run_cmd( cmdline, expected_ret_code=0 ):
    if expected_ret_code:
        print "\033[90mRunning: '%s' \t(expecting %s ret code)\033[0m" % ( cmdline, expected_ret_code )
    else:
        print "\033[90mRunning: '%s'\033[0m" % ( cmdline, )
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


current_script_dir = dirname( abspath( __file__ ) )
chdir( current_script_dir )

source_files = [
    "helloworld.tx",
    "array_equals.tx",
    "array_interface.tx",
    "string_test.tx",
    "scalars_test.tx",
    "for_loops.tx",
    "format_test.tx",
]

for src in source_files:
    run_cmd( """txc -quiet -jit -nobc -tx ../../ %s""" % ( src, ) )
