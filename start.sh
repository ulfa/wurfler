#!/bin/sh

erl +A 5 +K true -sname lilly +K true -pa $PWD/ebin $PWD/deps/*/ebin -s wurflerservice -boot start_sasl 
