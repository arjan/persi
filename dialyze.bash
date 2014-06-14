#!/bin/bash
DIRNAME=`pwd`
PROJNAME=`basename $DIRNAME`
GLOBALPLT="$HOME/.dialyzer_plt"

set -e

if [[ ! -f "$GLOBALPLT" ]];
then
    echo "OTP plt ($GLOBALPLT) not found"
    exit -1
fi


if [[ -d deps ]]; then 
    if [[ ! -f "deps.plt" ]]; then
        rebar compile
        echo "Dialyzing dependencies"
        dialyzer --add_to_plt --plt $GLOBALPLT --output_plt deps.plt -r deps/*/ebin/
    fi
    DEPSPLTARG="--plt deps.plt"
else
    DEPSPLTARG=""
fi

rebar compile skip_deps=true

if [[ -f $PROJNAME.plt ]]; then
    dialyzer --check_plt --plt $PROJNAME.plt -r ebin/
    if [[ $? -ne 0 ]]; then
	    echo "Not up to date, dialyzing"
	    dialyzer --add_to_plt $DEPSPLTARG --output_plt $PROJNAME.plt -r ebin/
    fi
else
    echo "Dialyzing $PROJNAME"
    dialyzer --add_to_plt $DEPSPLTARG --output_plt $PROJNAME.plt -r ebin/
fi

echo "Checking"
dialyzer -Werror_handling -Wrace_conditions -Wunderspecs --plt $PROJNAME.plt -r ebin/
