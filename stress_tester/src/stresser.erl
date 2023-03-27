-module(stresser).
-export([start_all/3]).
-compile(export_all).

%%
%% Each package will go through 10 facility-to-vehicle changes.
%% Each of these 10 vehicles will go through 1,000 location changes.
%%
start_all(Domain,Tester_name,Package_count)->
	Http_info = {post%Method
	 ,string:concat("https://",Domain)%URL
     ,[]%Header
     ,"application/json"%Type
     ,[{ssl, [{customize_hostname_check, 
                [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}]}]%HttpOptions
     ,[]},%Options
	build_and_spawn_all(Domain,Tester_name,Package_count,Http_info).

build_and_spawn_all(Domain,Tester_name,Package_count,Http_info)->
	{Package_ids,Facility_ids,Vehicle_ids,Package_history_changes} = data:build_package_history_adds(Package_count,Domain,Tester_name,[],[],[],[]),
	Facility_adds = data:build_facility_adds(Facility_ids),
	Package_history_queries = data:build_package_history_queries(Package_ids),
	Vehicle_history_queries = data:build_vehicle_history_queries(Vehicle_ids),
	Facility_queries = data:build_facility_queries(Facility_ids),
	spawn_facility_adds(Facility_adds,Http_info),
	spawn_package_history_changes(Package_history_changes,Http_info),
	spawn_facility_queries(Facility_queries, Http_info),
	spawn_vehicle_history_queries(Vehicle_history_queries, Http_info),
	spawn_package_history_queries(Package_history_queries, Http_info).


spawn_facility_queries(Facility_queries, Http_info)->
	[spawn(fun()-> send(Q,Http_info,"/query_facility") end) || Q <- Facility_queries].

spawn_vehicle_history_queries(Vehicle_history_queries, Http_info)->
	[spawn(fun()-> send(Q,Http_info,"/query_vehicle_history") end) || Q <- Vehicle_history_queries].

spawn_package_history_queries(Package_history_queries, Http_info)->
	[spawn(fun()-> send(Q,Http_info,"/query_package_history") end) || Q <- Package_history_queries].



spawn_facility_adds(Facilities,Http_info)->
	[spawn(fun()-> send(F,Http_info,"/store_facility_info") end) || F <- Facilities].

spawn_package_history_changes([],_)->done;
spawn_package_history_changes([Change|T],Http_info)->
	UrlExtension = case is_map_key(vehicle_uuid,Change) of
					true-> "/store_vehicle_info";
					_-> "/store_package_info"
				end,
	spawn(fun()-> send(Change,Http_info,UrlExtension) end),
	spawn_package_history_changes(T,Http_info).



send(Body,{Method,URL,Header,Type,HTTPOptions,Options},URL_extension)->
    httpc:request(Method, {string:concat(URL,URL_extension), Header, Type, jsx:encode(Body)}, HTTPOptions, Options).



