-module(jp).
-compile([export_all]).

%-define(debug,true).

-ifdef(debug).
-define(DBG(X), io:format("~p:~p: ~s~n", [?MODULE,?LINE,X])).
-else.
-define(DBG(X), true).
-endif.

tests_object_array_basic() ->
	[
	 	% {name, input, output}
	 	{ "Str1", "{\"A\":\"B\"}", [{"A","B"}] },
		{ "Str2", "{\"A\":\"B\",\"C\":\"D\"}", [{"A","B"},{"C","D"}] },
		{ "Bool1", "{\"A\":true}", [{"A",true}] },
		{ "Bool2", "{\"A\":true,\"B\":false}", [{"A",true},{"B",false}] },
		{ "Bool3", "{\"A\":true,\"B\":false,\"C\":true}", [{"A",true},{"B",false},{"C",true}] },
		{ "Num1", "{\"A\":1}", [{"A",1}] },
		{ "Num2", "{\"A\":1.1,\"B\":2.2}", [{"A",1.1},{"B",2.2}] },
		{ "Num3", "{\"A\":.1,\"B\":2.2,\"C\":3.14159}", [{"A",1.1},{"B",2.2},{"C",3.14159}] }
	].

fail_tests() ->
	[
		"{\"A\":}"
	].

run_test({Name,Input,ExpectedOutput}) ->
	%io:fwrite("Name: ~s~n",[Name]),
	case json_to_object_array(Input) of
		ExpectedOutput ->
			io:fwrite("~s passed~n",[Name]),
			pass;
		true ->
			io:fwrite("~s failed~n",[Name]),
			fail
	end.

run_tests(Ts) ->
	lists:map(fun run_test/1,Ts).


t() ->
	Tests = [
		_S7 = "{\"A\":{\"B\":\"b\"},\"C\":true}",
		_SNA1 = "{\"A\":[1]}",
		_SNA2 = "{\"A\":[1,2,3,4,5]}",
		_S4 = "{\"error\":[],\"result\":{\"unixtime\":1456604145,\"rfc1123\":\"Sat, 27 Feb 16 20:15:45 +0000\"}}",
		_S5 = "{\"A\":[\"B\",\"C\"],\"D\":33.2}",
		_S6 = "{\"varA\":\"valA\",\"varB\":\"valB\"}",
		_S8 = "{\"A\":[\"B\",\"C\"]}",
		_S9 = "{\"A\":[],\"B\":\"lol\"}",
		_S10 = "{\"A\":{\"B\":\"C\",\"D\":\"E\"}}",
		_S11 = "{\"A\":[1,2,3,4,5]}"
	],
	%pj(S2,[object_start,top]),
	Results = lists:map(
		fun(A) ->
			{A,json_to_object_array(A)}
		end,
		Tests
	),
	lists:map(
	   fun({Input,Output}) ->
			io:fwrite("~p ~n~n ~p~n~n~n",[Input,Output])
		end,
		Results
	).

get_str([H|T],S) ->
	case H of 
		str_e -> 
			{lists:reverse(S),T};
		C -> get_str(T,[C|S])
	end.

get_valstr([H|T],S) ->
	case H of 
		valstr_e -> 
			{lists:reverse(S),T};
		C -> get_valstr(T,[C|S])
	end.

get_num([H|T],S) ->
	case H of
		num_e ->
			try
				NumF  = list_to_float(lists:reverse(S)),
				{NumF,T}
			catch error:badarg ->
				try
					NumI  = list_to_integer(lists:reverse(S)),
					{NumI,T}
				catch error:badarg ->
					io:fwrite("Error converting ~w to number\n",[S]),
					{"num_conversion_error",T}
				end
			end;

		C -> get_num(T,[C|S])
	end.

get_array([H|T],Arr) ->
	case H of
		arr_s ->
			{Arr,T};
		H ->
			get_array(T,[H|Arr])
	end.

get_object([H|T],Arr) ->
	case H of
		obj_s ->
			{Arr,T};
		H ->
			get_object(T,[H|Arr])
	end.

json_to_object_array(String) ->
	case tokenize(String) of
		{success,Tokens} ->
			{success,token_stream_to_object_array(Tokens)};
		Error ->
			Error
	end.

json_get([],_Key) ->
	invalid_key;
json_get([{K,V}|_Js],Key) when K==Key -> V;
json_get([_X|Xs],Key) -> json_get(Xs,Key).

explain_error({error,X,ErrorMessage,FullString,StringRem,ParserStack}) ->
	io:fwrite("Error while tokenizing JSON, "),
	io:fwrite("expecting to see '~p'~n",[lists:nth(1,ParserStack)]),
	io:fwrite("but got '~s' instead~n",[StringRem]),
	io:fwrite("Error message was: \"~s\"~n",[ErrorMessage]),
	io:fwrite("Partial tokenization: ~p~n",[X]),
	io:fwrite("Input string: '~s'~n",[FullString]),
	io:fwrite("String remaining: '~s'~n",[StringRem]),
	io:fwrite("Parser stack: ~w~n",[ParserStack]);
explain_error(_) ->
	io:fwrite("There is no error, the parse was successful!\n").


token_stream_to_object_array(Tk) ->
	lists:nth(1,tk(Tk,[])).

tk([],Out) -> lists:reverse(Out);
tk([H|T],Out) ->
	?DBG(io_lib:format("Tok: ~w, Out: ~w",[[H|T],Out])),
	
	case H of
		obj_s -> 
			tk(T,[obj_s|Out]);
		obj_c ->
			tk(T,Out);
		str_s ->
			{S,NT} = get_str(T,[]),
			tk(NT,[S|Out]);

		valstr_s ->
			{S,NT} = get_valstr(T,[]),
			tk(NT,[S|Out]);

		obj_e ->
			{Arr,OutTail} = get_object(Out,[]),
			tk(T,[Arr|OutTail]);

		arr_s ->
			tk(T,[arr_s|Out]);

		arr_e ->
			{Arr,OutTail} = get_array(Out,[]),
			tk(T,[Arr|OutTail]);

		arr_sep ->
			tk(T,Out);

		key_sep ->
			tk(T,Out);
		
		val_e ->
			[Value|OutTail] = Out,
			[Key|OutTailTail] = OutTail,
			?DBG(io_lib:format("got key: ~p and value ~p",[Key,Value])),
			tk(T,[{Key,Value}|OutTailTail]);

		num_s ->
			{Num,NT} = get_num(T,[]),
			tk(NT,[Num|Out]);

		true ->
			tk(T,[true|Out]);

		false ->
			tk(T,[false|Out]);

		[] ->
			?DBG("Done"),
			?DBG(io_lib:format("~p",[Out])),
			[]
	end.

is_digit_char(C) ->
	case C of
		$0 -> true;
		$1 -> true;
		$2 -> true;
		$3 -> true;
		$4 -> true;
		$5 -> true;
		$6 -> true;
		$7 -> true;
		$8 -> true;
		$9 -> true;
		$. -> true;
		_  -> false
	end.

get([],_KeyName) -> [];
get([H|T],KeyName) ->
	{Name,Value} = H,
	if 
		Name==KeyName -> Value;
		true -> get(T,KeyName)
	end.


tokenize(S) ->
	Result = pj(S,[object]),
	io:fwrite("~w",[Result]),
	case lists:last(Result) of
		{error,ErrorMsg,StringRem,Stack} ->
			{error,lists:droplast(Result),ErrorMsg,S,StringRem,Stack};
		obj_e ->
			{success,Result}
	end.

pj([],S) ->
	?DBG("DONE"),
	S;
pj(S,[State|RestState]) ->
	?DBG(io_lib:format("~p",[[State|RestState]])),
	?DBG(S),
	case State of
		object ->
			pj(S,[ws,object_start,ws,key_value_pair,ws,object_end|RestState]);

		key_value_pair ->
					pj(S,[string,ws,object_colon,ws,value|RestState]);

		ws ->
			case S of
				"\r"++Rest -> pj(Rest,[State|RestState]);
				"\n"++Rest -> pj(Rest,[State|RestState]);
				" " ++Rest -> pj(Rest,[State|RestState]);
				[] ->
					io:fwrite("Unexpected end of string!\n"),
					[];
				_O -> pj(S,RestState)
			end;

		object_start ->
			case S of
				"{"++Rest ->
					?DBG("OBJECT BEGIN"),
					[obj_s | pj(Rest,RestState)];
				[_H|_Rest] ->
					?DBG("ERROR: Unexpected item"),
					[];
				[] ->
					EMsg = "ERROR: Unexpected end of string!",
					?DBG(EMsg),
					[{error,EMsg}]
			end;

		object_colon ->
			case S of
				":"++Rest ->
					?DBG("OBJECT COLON"),
					[obj_c|pj(Rest,RestState)];
				[_H|_Rest] ->
					io:fwrite("Unexpected item\n"),
					[];
				[] ->
					io:fwrite("Unexpected end of string!\n"),
					[]
			end;

		key_separator ->
			case S of
				" "++Rest ->
					pj(Rest,[State|RestState]);
				","++Rest ->
					?DBG("Got key separator"),
					[key_sep|pj(Rest,[string_start|[object_colon|[object_end|RestState]]])];
				[] -> 
					?DBG("ks end of string"),
					pj(S,RestState);
				[_H|_Rest] ->
					pj(S,RestState)
			end;

		object_end ->
			case S of
				"}"++Rest ->
					?DBG("OBJECT END"),
					[obj_e | pj(Rest,RestState)];
				","++Rest ->
					pj(Rest,[key_value_pair|[State|RestState]]);
				[_H|_Rest] ->
					EMsg = "object_end: Unexpected item",
					?DBG(EMsg),
					[{error,EMsg,S,[State|RestState]}];
				[] -> 
					EMsg = "object_end: end of string",
					?DBG(EMsg),
					[{error,EMsg,S,[State|RestState]}]
			end;

		string ->
			pj(S,[string_start,string_end|RestState]);

		string_start ->
			case S of
				"\""++Rest -> 
					?DBG("STRING BEGIN"),
					[str_s|pj(Rest,RestState)];
				_O ->
					EMsg = "Expected string start!",
					?DBG(EMsg),
					[{error,EMsg,S,[State|RestState]}]
			end;

		string_end ->
			case S of 
				"\""++Rest -> 
					?DBG("STRING END"),
					[str_e|pj(Rest,RestState)];
				[C|Rest] ->
					[C | pj(Rest,[State|RestState])]
			end;

		valstring_start ->
			case S of
				"\""++Rest -> 
					?DBG("VALUE STRING BEGIN"),
					[valstr_s|pj(Rest,[valstring_end|RestState])]
			end;

		valstring_end ->
			case S of 
				"\""++Rest -> 
					?DBG("VALUE STRING END"),
					[valstr_e|pj(Rest,RestState)];
				[C|Rest] ->
					[C | pj(Rest,[State|RestState])]
			end;

		value_end ->
			[val_e|pj(S,RestState)];

		array_value ->
			pj(S,[value_start|RestState]);

		value ->
			pj(S,[value_start,value_end|RestState]);

		value_start ->
			case S of
				"\""++_Rest -> 
					?DBG("got string value"),
					pj(S,[string|RestState]);
				"{"++_Rest ->
					?DBG("got object value"),
					pj(S,[object|RestState]);
				"true"++Rest ->
					[true|pj(Rest,RestState)];
				"false"++Rest ->
					[false|pj(Rest,RestState)];
				"["++_Rest ->
					pj(S,[array|RestState]);
				[D|_Rest] ->
					case is_digit_char(D) of
						true ->
							?DBG("matched digit"),
							[num_s|pj(S,[number|RestState])];
						false ->
							pj(S,RestState)
							%EMsg = "Value not recognised",
							%?DBG(EMsg),
							%[{error,EMsg,S,[State|RestState]}]
					end
			end;

		number ->
			case S of
				[D|Rest] ->
					case is_digit_char(D) of
						true ->
							?DBG("matched digit"),
							[D|pj(Rest,[number|RestState])];
						false ->
							[num_e|pj(S,RestState)]
					end
			end;

		array ->
			pj(S,[array_start,ws,array_value,ws,array_end|RestState]);

		array_start ->
			case S of
				"["++Rest ->
					?DBG("array start"),
					[arr_s|pj(Rest,RestState)];
				_O ->
					io:fwrite("Expected Array start, got garbage\n")
			end;

		array_end ->
			case S of
				"]"++Rest ->
					?DBG("array end"),
					[arr_e|pj(Rest,RestState)];
				","++Rest ->
					?DBG("array sep"),
					[arr_sep|pj(Rest,[array_value,array_end|RestState])]
			end

	end.
