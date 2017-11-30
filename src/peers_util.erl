-module(peers_util).
-compile(export_all).


convert_peers_dets_to_json(Path) ->
	{ok, peers} = dets:open_file(peers, [{file,Path}]),

	Maps = dets:foldl(fun(
		{
			IP_Address,
			UserAgent,
			Services,
			BestHeight,
			LastUseTime,
			TotalUseDuration,
			TotalInBytes,
			TotalOutBytes,
			LastError
		},AccIn) ->
			[#{
				ip_address=>to_binary(IP_Address),
				user_agent=>UserAgent,
				services=>Services, % array
				best_height=>BestHeight,
				last_use_time=>LastUseTime,
				total_use_duration=>TotalUseDuration,
				total_in_bytes=>TotalInBytes,
				total_out_bytes=>TotalOutBytes,
				last_error=>to_binary(LastError)
			}|AccIn]
		end,
		[], % Acc0
		peers),
	
	Json = jsone:encode(Maps),
	
	ok = file:write_file("./peers.json", Json).


to_binary(Any) ->
	list_to_binary(io_lib:fwrite("~w",[Any])).



