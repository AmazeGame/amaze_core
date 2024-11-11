%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 11月 2024 23:40
%%%-------------------------------------------------------------------
-module(amaze_frame_numeric).
-include("amaze_gateway.hrl").
%% API
-export([pack/1,unpack/1]).
-export([name/0]).

-define(NUMERIC_END_CHAR,$.).
-define(NUMERIC_END_BINARY,<<$.>>).

%%-compile(export_all).

-spec name()-> atom().
name()->
	numeric.



-spec pack(binary()|[binary()])-> {ok,binary()}|{ok,iolist()} | {ok,[binary()],ErrorBinary::term()}.
pack(<<>>)->
	<<0:8>>;
pack(Binary) when is_binary(Binary)->
	BinaryLength = byte_size(Binary),
	PackLen = pack_length(BinaryLength),
	{ok,<<PackLen/binary,Binary/binary>>};
pack(Binaries) when is_list(Binaries)->
	PackedList = lists:foldl(
		fun
			(Binary,Acc)->
				{ok,PackedBinary} =  pack(Binary),
				[PackedBinary|Acc]
		end,[], Binaries),
	{ok, lists:reverse(PackedList)}.


-spec unpack(binary())-> {ok,[binary()]} |{ok,[binary()],binary()} | {?PARTITION_KEY,binary()} | {error,binary()}.
unpack(PackBinary) ->
	case binary:split(PackBinary,?NUMERIC_END_BINARY) of
		[_PackBinary0]->
			case catch binary_to_integer(PackBinary) of
				{'EXIT',_}-> {error,PackBinary};
				_Length when _Length<0 ->
					{error,PackBinary};
				_Length ->
					{?PARTITION_KEY,PackBinary}
			end;
		[PackBinary0,LeftBinary]->
			case catch binary_to_integer(PackBinary0) of
				{'EXIT',_}-> {error,PackBinary};
				Length ->
					LeftBinaryLength = byte_size(LeftBinary),
					if
						Length> LeftBinaryLength->
							{?PARTITION_KEY,PackBinary};
						Length == LeftBinaryLength->
							{ok,[LeftBinary]};
						true->
							FirstFullBinary = binary:part(LeftBinary,0,Length),
							LeftBinary0 = binary:part(LeftBinary,Length,LeftBinaryLength - Length),
							case unpack(LeftBinary0) of
								{ok,Unpacked}->
									{ok,[FirstFullBinary|Unpacked]};
								{ok,Unpacked,{error,ErrorBinary}}->
									{ok,[FirstFullBinary|Unpacked],{error,ErrorBinary}};
								{ok,Unpacked,{?PARTITION_KEY,PartitionBinary}}->
									{ok,[FirstFullBinary|Unpacked],{?PARTITION_KEY,PartitionBinary}};
								{?PARTITION_KEY,PartitionBinary}->
									{ok,[FirstFullBinary],{?PARTITION_KEY,PartitionBinary}};
								{error,_}->
									{ok,[FirstFullBinary],{error,LeftBinary0}}
							end
					end
			end
	end.

pack_length(0)->
	<<$0,?NUMERIC_END_CHAR>>;
pack_length(Length) when is_integer(Length)->
	LenBinary = integer_to_binary(Length),
	<<LenBinary/binary,?NUMERIC_END_CHAR>>.

