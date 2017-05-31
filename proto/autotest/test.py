from os import listdir
from os.path import dirname, abspath, isfile, join
from subprocess import call
import sys


def run_python( file ):
    run_cmd( "python " + file )

def run_cmd( cmdline, expected_ret_code=0 ):
    print "\033[4;90mRunning: '%s'\033[0m" % cmdline
    try:
        retcode = call( cmdline, shell=True )
        if retcode < 0:
            print >>sys.stderr, "\033[0;41mTest failed, child was terminated by signal %s\033[0m" % ( -retcode, )
        elif retcode != expected_ret_code:
            print >>sys.stderr, "\033[0;41mExpected return code %s but was %s\033[0m" % ( expected_ret_code, retcode )
    except OSError as e:
        print >>sys.stderr, "Execution failed:", e 
        raise


current_script_dir = dirname( abspath( __file__ ) )

files = []
dirs = []
for name in listdir(current_script_dir):
    if isfile(name):
        files.append(name)
    else:
        dirs.append(name)

for dir in sorted( dirs ):
    dir_path = join( current_script_dir, dir )
    test_script = join( dir_path, "test.py" )
    if isfile( test_script ):
        run_python( test_script )
