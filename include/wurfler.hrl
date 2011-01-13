-define(DEFAULT_TIMESTAMP, "01.01.1970").
-record(capability, {name, value}).
-record(group, {id, capabilites=[]}).
-record(device, {id, user_agent, actual_device_root, fall_back, brand_name=undefined, model_name=undefined, groups=[], created, lastmodified, deleted=false}).
-record(index, {device, table=[]}).
-record(brand_index, {brand_name, models=[]}).
-record(capabilities_devices, {capabilities, devices, created}).
-record(capability_description, {name, type, description}).
-define(CONTAINS, fun({device, [{model_name, Model_Name},_], []}) ->					   					   
					if Device#device.model_name == Model_Name -> true;
					    true -> false
					end
			   	  end).
