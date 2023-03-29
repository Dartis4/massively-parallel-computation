-module(data).
-export([build_facility_adds/1,build_package_history_queries/1,
		 build_vehicle_history_queries/1,build_facility_queries/1,
		 build_package_history_adds/7, build_switch/4]).

%%
%% Here are the data transfer definitions used in this version of the app
%%

%Extension: /store_package_infoData D
	%to server: {"package_uuid":"string","holder_uuid":"string","time_stamp":"UINT_32"}
%Extension: /query_package_history
	%to server: {"package_uuid":"string"}
	%out from server: {"history":[{"holder_uuid":"string","time_stamp":"UINT_32"}]}
%Extension: /store_vehicle_info
	%to server: {"vehicle_uuid":"string","location":{"lat":"real","long":"real"},"time_stamp":"UINT_32"}
%Extension: /query_vehicle_history
	%to server: {"vehicle_uuid":"string"}
	%out from server: {"history":[{"location":{"lat":"real","long":"real"},"time_stamp":"UINT_32"}]}
%Extension: /store_facility_info
	%to server: {"facility_uuid":"string","city":"string"}Extension/query_facilityData DescriptionIn{"facility_uuid":"string"}
	%out from server: {"city":"string"}


time_in_millis() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

 shuffle_list(List)->
 	[Element||{_,Element} <- lists:sort([ {rand:uniform_real(), E} || E <- List])].

 

generate_UUID(Domain,Tester_name)->
	uuid:to_string(uuid:uuid5(dns,Domain++atom_to_list(Tester_name)++integer_to_list(erlang:monotonic_time()))).


build_facility_adds(UUIDs)->
	[#{facility_uuid => list_to_binary(UUID),city=>list_to_binary("Rexburg,Idaho 83440")} || UUID <- UUIDs].

build_package_history_adds(0,_Domain,_Tester_name,Package_id_accum,
							Facility_id_accum,Vehicle_id_accum,History_accum)-> 
	{Package_id_accum,Facility_id_accum,Vehicle_id_accum,History_accum};
build_package_history_adds(Package_count,Domain,Tester_name,Package_id_accum,
							Facility_id_accum,Vehicle_id_accum,History_accum)->
	Package  = generate_UUID(Domain,Tester_name),
	%make a list of facility/vehicle tuples
	Facility_vehicle_pairs = [{generate_UUID(Domain,Tester_name),generate_UUID(Domain,Tester_name)} 
								|| _ <- lists:seq(1,10)],
	%merge all of the switch lists into one list
	Changes = lists:merge([data:build_switch(76,F,V,Package) || {F,V} <- Facility_vehicle_pairs]),
	{Facilities,Vehicles} = lists:unzip(Facility_vehicle_pairs),
	build_package_history_adds(Package_count-1,Domain,Tester_name,
		[Package]++Package_id_accum,Facilities++Facility_id_accum,Vehicles++Vehicle_id_accum,History_accum++Changes).


build_switch(Vehicle_move_count,Facility,Vehicle,Package)->
	%move the package into the facility
	[#{package_uuid=>list_to_binary(Package),holder_uuid=>list_to_binary(Facility),time_stamp=>time_in_millis()}]++
	%move the package into the vehicle
	[#{package_uuid=>list_to_binary(Package),holder_uuid=>list_to_binary(Vehicle),time_stamp=>time_in_millis()}]++
	%move the vehicle 'Vehicle_move_count' number of times
	[#{vehicle_uuid=>list_to_binary(Vehicle),location=>#{lat=>rand:uniform_real()*360,
														long=>rand:uniform_real()*360},
														time_stamp=>time_in_millis()}
														|| _ <- lists:seq(1,Vehicle_move_count)].
%%
%% If the queries below had been generalized into one that included no type information,
%% there would only be one of these functions.
%%

%%
%% makes 10 queries for each package. The order of the queries is randized.
%%
build_package_history_queries(UUIDs)->
	shuffle_list(lists:merge(lists:duplicate(10,[#{package_uuid=>list_to_binary(UUID)} || UUID <- UUIDs]))).

%%
%% makes 10 queries for each vehicle. The order of the queries is randized.
%%
build_vehicle_history_queries(UUIDs)->
	shuffle_list(lists:merge(lists:duplicate(10,[#{vehicle_uuid=>list_to_binary(UUID)} || UUID <- UUIDs]))).

%%
%% makes 10 queries for each facility. The order of the queries is randized.
%%
build_facility_queries(UUIDs)->
	shuffle_list(lists:merge(lists:duplicate(10,[#{facility_uuid=>list_to_binary(UUID)} || UUID <- UUIDs]))).




	

