# actual version

Wurfler is not only a wrapper around wurfl, so you can query the data by user agent agent.

# arichitecture


## Feature which are implemented and working


## serching for something
### import of devices
### export of devices
### notifictian of changed devices



## Installation

First you have to run install.sh which creates the schema and the db tables.

## Data import

To import data you have to follow these steps:

### wurfl file

In the directory test there is a wurfl-2.0.25.xml file. Please, rename it and copy
this file in the directory wurfl. Everything else will be done by the system. 
If this is not available, you have to create it.Normaly, it will be created
using the install.sh script.


### wurfl_patch file

To insert new data or to modify existing data you have to use the wurfl_patch.xml.
After creating a wurfl_patch.xml file you can copy this in the wurfl_patch directory.
Everything else will be done by the system.

## Runing common_test

Before running the following line you have to modify the cover.spec and test.spec.
Inside these files you have to specify your own absolute path. Later i will build
a script which will do the work for you.
In the cover.spec you also have to specify the name of your node. 
Note: If ct_run is not in the PATH you have to create the link. 
In /usr/local/bin :
sudo ln -s ../lib/erlang/bin/ct_run ct_run

Then you can run : 

ct_run -spec test/test.spec -pa deps/*/ebin/ ./ebin 

To start the server 

ct_run -vts -spec test/test.spec -pa deps/*/ebin/ ./ebin -dir test  