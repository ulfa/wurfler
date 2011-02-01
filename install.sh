#!/bin/sh

cat $PWD/data/test.spec.in | sed -e "s,@PATH@,$PWD," > $PWD/test/test.spec
cat $PWD/data/cover.spec.in | sed -e "s,@PATH@,$PWD," > $PWD/test/cover.spec

if [ ! -d wurfl ]		# be sure the directory wurfl exists
then
   mkdir wurfl
fi

if [ ! -d wurfl_patch ]		# be sure the directory wurfl_patch exists
then
   mkdir wurfl_patch
fi

if [ ! -d logs ]		# be sure the directory logs exists
then
   mkdir logs
fi


erl -name wurflerservice@127.0.0.1 +K true -pa $PWD/ebin $PWD/deps/*/ebin -s wurfler_db -s init stop
