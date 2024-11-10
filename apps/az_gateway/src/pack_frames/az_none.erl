%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 11月 2024 23:40
%%%-------------------------------------------------------------------
-module(az_none).
-export([pack/1,unpack/1]).
-export([name/0]).
-include("../../include/az_gateway.hrl").
-spec name()-> atom().
name()->
	none.

-spec pack(binary()|[binary()])-> {ok,binary()}|{ok,iolist()} | {error,binary()}.
pack(Binary) when is_binary(Binary)->
	{ok,Binary};
pack(Binaries) when is_list(Binaries)->
	{PackedList,ErrList} = lists:foldl(
		fun
			(Binary,{Acc,[]})->
				case pack(Binary) of
					{ok,PackedBinary}->
						{[PackedBinary|Acc]};
					{ok,PackedBinary,ErrUnpackedBinary}->
						{[PackedBinary|Acc],[ErrUnpackedBinary]}
				end;
			(Binary,{Acc,AccErrs})->
				{Acc,[Binary|AccErrs]}
		end,{[],[]}, Binaries),
	if
		ErrList=/=[]->
			{ok, lists:reverse(PackedList),lists:reverse(ErrList)};
		true->
			{ok, lists:reverse(PackedList)}
	end.


-spec unpack(binary())-> {ok,[binary()]} |{ok,[binary()],binary()} | {?PARTITION_KEY,binary()} | {error,binary()}.
unpack(Binary) ->
	{ok,[Binary]}.