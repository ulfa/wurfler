# First version

In the frist version of the wurfler you can query by user-agent and you can
query by capabilities. Also, it is possible to get information of device by
sending the device id to the service.

If the test/wurfltest.xml is configurerd than you can test it with :

curl -H "Accept: text/xml" -v http://localhost:8000/device/rocker

and 

curl -d '@test/xml_caps_request.xml' -H "Accept: text/xml" -v http://localhost:8000/devices

or

curl -H "Accept: text/xml" -A "rocker_ua" -v http://localhost:8000/device

The first request will send a get request to the service and will return the information 
of the device with the id.

The second post will send capabilities to the service and will return the devices which
have the capabilities.
In the current version the information model_name and brand_name is missing. 

The third request will analyze the given user agent and will return the device information.

# Migration to mnesia

The new version uses mnesia instead of ets tables.

## Installation

First you have to run install.sh which creates the schema and the db tables.

## Runing common_test

Before running the following line you have to modify the cover.spec and test.spec.
Inside these files you hae to specify your own absolute path. Then you can run : 

ct_run -spec test/test.spec -pa deps/*/ebin/ ./ebin -cover test/cover.spec 