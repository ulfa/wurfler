#!/bin/sh

erl +A 5 +K true -name wurflerservice@127.0.0.1 -pa $PWD/ebin $PWD/test $PWD/deps/*/ebin -boot start_sasl -s reloader -s toolbar -s wurflerservice
