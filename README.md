# actual version


## Feature which are implemented and working

### get device by id
 
request:  

<code>
curl -H "Accept: text/xml" -v http://localhost:8000/device/generic
</code>

response:

<device id="generic" user_agent="" actual_device_root="undefined" fall_back="root"><group id="product_info">
	<capability name="device_os" value=""/>
	<capability name="nokia_series" value="0"/>
</device>


### get device by user agent

request:

<code>
curl -H "Accept: text/xml" -A "AUDIOVOX-CDM180" -v http://localhost:8000/device
</code>

response:

<code>
<device id="generic" user_agent="" actual_device_root="undefined" fall_back="root"><group id="product_info">
	<capability name="device_os" value=""/>
	<capability name="nokia_series" value="0"/>
</device>
</code>

### get devices by capabilities

request:
<code>
curl -d '@test/xml_caps_request.xml' -H "Accept: text/xml" -v http://localhost:8000/devices
</code>

where the content of xml_caps_request.xml is : 

<code>
<?xml version="1.0" encoding="utf-8"?>
<query>
	<capabilities>
		<capability name="device_os" value="iPhone OS" operator="="/>
		<capability name="device_os_version" value="0.0" operator=">"/>
	</capabilities>
</query>
</code>

response:

<code>
<devices>
	<device model_name="iPad" brand_name="Apple"/>
	<device model_name="QuickTime Agent" brand_name="Apple"/>
	<device model_name="iPhone" brand_name="Apple"/>
	<device model_name="iPod Touch" brand_name="Apple"/>
</devices>
</code>

### get devices by model name


request: 

<code>
curl -H "Accept: text/xml" -v http://localhost:8000/model/MB200
</code>

response:

<code>
<devices>
	<device model_name="MB200" brand_name="Motorola"/>
</devices>
</code>

### get all brands

request:
 
<code>
curl -H "Accept: text/xml" -v http://localhost:8000/brands
</code>

response:

<code>
<brand name="Motorola">
	<model id="mot_w377g_ver1" model_name="MOT-W377g"/>
	<model id="mot_ex300_ver1" model_name="EX300"/>
</brand>
</code>

### get brand by brand name

request: 

<code>
curl -H "Accept: text/xml" -v http://localhost:8000/brand/Motorola
</code>

response:

<code>
<brand name="Motorola">
	<model id="mot_w377g_ver1" model_name="MOT-W377g"/>
	<model id="mot_ex300_ver1" model_name="EX300"/>
</brand>
</code>



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

Then you can run : 

ct_run -spec test/test.spec -pa deps/*/ebin/ ./ebin 

To start the server 

ct_run -vts -spec test/test.spec -pa deps/*/ebin/ ./ebin -dir test  