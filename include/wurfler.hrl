-record(capability, {name, value}).
-record(group, {id, capabilites=[]}).
-record(device, {id, user_agent, actual_device_root, fall_back, brand_name=undefined, model_name=undefined, groups=[], created, lastmodified}).
-record(index, {device, table=[]}).
-record(brand_index, {brand_name, models=[]}).
-define(CONTAINS, fun({device, [_,{model_name, Model_Name},_], []}) ->					   					   
					if 
						Device#device.model_name == Model_Name -> true;
					    true -> false
					end
			   end).