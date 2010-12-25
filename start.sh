#!/bin/sh

erl -sname lilly +K true -pa $PWD/ebin $PWD/deps/*/ebin -s wurflerservice -boot start_sasl -s toolbar
