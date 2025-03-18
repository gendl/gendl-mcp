#!/bin/bash

set -e

if [ -e /opt/gendl ]
then

    /opt/gendl/gdl/program/gdl-ccl -n --batch -l ./load.lisp -e "(ccl:quit)"
    
    echo User is: `id -u`
fi

set +e     
