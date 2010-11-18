#!/bin/sh

erl +K true -pa $PWD/ebin $PWD/deps/*/ebin -s wurflerservice -s tv -s appmon
