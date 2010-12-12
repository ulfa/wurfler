#!/bin/sh
mkdir wurfl
mkdir wurfl_patch
mkdir logs
erl -sname lilly +K true -pa $PWD/ebin $PWD/deps/*/ebin -s wurfler_db -s init stop
