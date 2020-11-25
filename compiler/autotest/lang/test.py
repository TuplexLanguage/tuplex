from os import listdir, chdir
from os.path import dirname, abspath, isfile, join
from subprocess import call
import sys


def run_python( file ):
    run_cmd( "python3 " + file )

def run_cmd( cmdline, expected_ret_code=0 ):
    if expected_ret_code:
        print ( "\033[90mRunning: '%s' \t(expecting %s ret code)\033[0m" % ( cmdline, expected_ret_code ) )
    else:
        print ( "\033[90mRunning: '%s'\033[0m" % ( cmdline, ) )
    try:
        retcode = call( cmdline, shell=True )
        if retcode < 0:
            print ( "\033[0;41mTest failed, child was terminated by signal %s\033[0m" % ( -retcode, ), file=sys.stderr )
        else:
            if expected_ret_code == "nonzero":
                if retcode == 0:
                    print ( "\033[0;41mExpected nonzero return code but was %s\033[0m" % ( retcode, ), file=sys.stderr )
            elif retcode != expected_ret_code:
                print ( "\033[0;41mExpected return code %s but was %s\033[0m" % ( expected_ret_code, retcode ), file=sys.stderr )
    except OSError as e:
        print ( "Execution failed: %s" % ( e, ), file=sys.stderr )
        raise


current_script_dir = dirname( abspath( __file__ ) )
chdir( current_script_dir )

run_cmd( """echo "module my" | txc -quiet -nojit -nobc -notx 2>/dev/null""" )
run_cmd( """echo "module tx" | txc -quiet -nojit -nobc -notx 2>/dev/null""", 2 )        # illegal namespace for user code
run_cmd( """echo "module tx.c" | txc -quiet -nojit -nobc -notx 2>/dev/null""", 2 )      # illegal namespace for user code
run_cmd( """echo "module tx.foobar" | txc -quiet -nojit -nobc -notx 2>/dev/null""", 2 ) # illegal namespace for user code
run_cmd( """echo "module \$local" | txc -quiet -nojit -nobc -notx 2>/dev/null""", 1 )   # can't be specified explicitly
run_cmd( """echo "module foo\$bar" | txc -quiet -nojit -nobc -notx 2>/dev/null""", 1 )  # illegal character

run_cmd( """txc -quiet -nojit -nobc -notx errtest.tx inttest.tx 2>/dev/null""", 2 )     # can't have more than one '$local' sources


source_files = [
    "helloworld.tx",

    "errtest.tx",
    "mainsignaturetest.tx",

    "inttest.tx",
    "floattest.tx",
    "declsyntaxtest.tx",
    "globalstest.tx",
    "equalstest.tx",
    "ifelsetest.tx",
    "whiletest.tx",

    "binoptest.tx",
    "arithmetictest.tx",

    "funcdeclsyntaxtest.tx",
    "funclogictest.tx",
    "funcrettest.tx",
    "funcoverloadtest.tx",
    "funcrecursetest.tx",
    "funcvarargs.tx",

    "test.modul.tx",
    "importtest.tx",

    "reftest.tx",
    "arraybasictest.tx",
    "arraydynamictest.tx",
    "arrayinittest.tx",
    "arraymodifiability.tx",

    "lookuptest.tx",
    "membertest.tx",
    "membermodifiability.tx",
    "polymorphtest.tx",
    "recursivetypetest.tx",
    "constructortest.tx",
    "interfacetest.tx",
    "interfaceadaptertest.tx",
    "nestedtypestest.tx",
    "typecasting.tx",
    "deletetest.tx",

    "generictypespec.tx",
    "genericvaluespec.tx",
    "genericmodifiability.tx",
    "genericnested.tx",
    "genericrefbound.tx",
    "genericstest.tx",

    "sequenceiftest.tx",
    "rangeiftest.tx",
    "genericconvtest.tx",

    "matrixtest.tx",

    "syntax_intro.tx",
]

for src in source_files:
    run_cmd( """txc -quiet -jit -nobc -notx %s""" % ( src, ) )

#for src in source_files:
#    run_cmd( """txc -quiet -jit -nobc -tx ../.. %s""" % ( src, ) )
