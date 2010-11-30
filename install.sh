#!/bin/sh

erl +K true -pa $PWD/ebin $PWD/deps/*/ebin -s wurfler_db -mnesia dir 'db' -s init stop
