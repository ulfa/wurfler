-record(capability, {name, value}).
-record(group, {id, capabilites=[]}).
%%-record(device, {id, user_agent, actual_device_root, fall_back, groups=[]}).
-record(device, {id, user_agent, actual_device_root, fall_back, brand_name=undefined, model_name=undefined, groups=[]}).