#!/bin/sh

erl +A 5 +K true -name wurflerservice@127.0.0.1 -pa $PWD/ebin $PWD/deps/*/ebin -s wurflerservice -boot start_sasl 
