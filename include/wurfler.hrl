-define(DEFAULT_TIMESTAMP, "01.01.1970").
-define(LOCATION, "Location").
-define(HOST, "host").
-record(capability, {name, value}).
-record(group, {id, capabilites=[]}).
-record(device, {id, user_agent=[], actual_device_root=false, fall_back=[],
				 brand_name=[], model_name=[], groups=[],
				 created=wurfler_date_util:get_uc_time(), lastmodified=wurfler_date_util:get_uc_time(), filter=[]}).
-record(index, {device, table=[]}).
-record(brand_index, {brand_name, models=[]}).
-record(capabilities_devices, {capabilities=[], devices=[], key,  created=wurfler_date_util:get_uc_time(), lastmodified=wurfler_date_util:get_uc_time()}).
-record(capability_description, {name, type, description}).
-record(os_device_id, {device_os, os_reg, device_ids=[]}).
-record(etag_cache, {id, term}).
