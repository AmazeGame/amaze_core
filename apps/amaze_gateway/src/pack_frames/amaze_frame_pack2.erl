%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 11月 2024 23:40
%%%-------------------------------------------------------------------
-module(amaze_frame_pack2).

%% API
-export([pack/1,unpack/1]).
-export([name/0]).
-include("amaze_gateway.hrl").
-spec name()-> atom().
name()->
	pack2.

-spec pack(binary()|[binary()])-> {ok,binary()}|{ok,iolist()} | {ok,[binary()],ErrorBinary::term()}.
pack(Binary) when is_binary(Binary)->
	BinaryLen = byte_size(Binary),
	if
		BinaryLen>= 65535->
			{ok,<<BinaryLen:16/unsigned-big-integer,Binary/binary>>};
		true->
			{ok,[],Binary}
	end;
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
unpack(<<>>) ->
	{error,<<>>};
unpack(<<_Length:8/unsigned-big-integer>> = Binary) ->
	{?PARTITION_KEY,Binary};
unpack(<<Length:16/unsigned-big-integer,PackBinary/binary>> = OriginBinary) ->
	do_unpack(Length,PackBinary,OriginBinary).

do_unpack(Length,PackBinary,OriginBinary)->
	PackBinLength = byte_size(PackBinary),
	if
		Length > PackBinLength->
			{?PARTITION_KEY,OriginBinary};
		Length == PackBinLength->
			{ok,[PackBinary]};
		true->
			FirstFullBinary = binary:part(PackBinary,0,Length),
			LeftBinary = binary:part(PackBinary,Length,PackBinLength - Length),
			case unpack(LeftBinary) of
				{ok,Unpacked}->
					{ok,[FirstFullBinary|Unpacked]};
				{ok,Unpacked,{error,ErrorBinary}}->
					{ok,[FirstFullBinary|Unpacked],{error,ErrorBinary}};
				{ok,Unpacked,{?PARTITION_KEY,PartitionBinary}}->
					{ok,[FirstFullBinary|Unpacked],{?PARTITION_KEY,PartitionBinary}};
				{?PARTITION_KEY,PartitionBinary}->
					{ok,[FirstFullBinary],{?PARTITION_KEY,PartitionBinary}};
				{error,_}->
					{ok,[FirstFullBinary],{error,LeftBinary}}
			end
	end.
