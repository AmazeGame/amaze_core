%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 11月 2024 23:40
%%%-------------------------------------------------------------------
-module(amaze_frame_mqtt).
-include("amaze_gateway.hrl").
%% API
-export([pack/1,unpack/1]).
-export([name/0]).


%%-compile(export_all).

-spec name()-> atom().
name()->
	mqtt.

-spec pack(binary()|[binary()])-> {ok,binary()}|{ok,iolist()} | {ok,[binary()],ErrorBinary::term()}.
pack(<<>>)->
	<<0:8>>;
pack(Binary) when is_binary(Binary)->
	BinaryLen = byte_size(Binary),
	case pack_length(BinaryLen) of
		error->
			{ok,[],[Binary]};
		PackLen->
			{ok,<<PackLen/binary,Binary/binary>>}
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
unpack(<<0:8>>) ->
	{ok,[<<>>]};
unpack(<<1:1,_Length:7>> = OriginBinary)->
	{?PARTITION_KEY,OriginBinary};
unpack(<<1:1,_Length0:7,1:1,_Length1:7 >> = OriginBinary)->
	{?PARTITION_KEY,OriginBinary};
unpack(<<1:1,_Length0:7,1:1,_Length1:7, 1:1,_Length2:7>> = OriginBinary)->
	{?PARTITION_KEY,OriginBinary};
unpack(<<0:1,Length:7,PackBinary/binary>> = OriginBinary)->
	do_unpack(Length,PackBinary,OriginBinary);
unpack(<<1:1,Length0:7,0:1,Length1:7, PackBinary/binary>> = OriginBinary)->
	Length = Length0 bor (Length1 bsl 7),
	do_unpack(Length,PackBinary,OriginBinary);
unpack(<<1:1,Length0:7,1:1,Length1:7, 0:1,Length2:7, PackBinary/binary>> = OriginBinary)->
	Length = Length0 bor (Length1 bsl 7) bor (Length2 bsl 14),
	do_unpack(Length,PackBinary,OriginBinary);
unpack(<<1:1,Length0:7,1:1,Length1:7, 1:1,Length2:7, 0:1,Length3:7 ,PackBinary/binary>> = OriginBinary)->
	Length = Length0 bor (Length1 bsl 7) bor (Length2 bsl 14) bor (Length3 bsl 21),
	do_unpack(Length,PackBinary,OriginBinary);
unpack(OriginBinary)->  %% 长度超出4个字节
	{error,OriginBinary}.

do_unpack(Length,PackBinary,OriginBinary)->
	PackBinLength = byte_size(PackBinary),
	if
		Length> PackBinLength->
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
				{error,_}-> %% 长度超出4个字节
					{ok,[FirstFullBinary],{error,LeftBinary}}
			end
	end.

pack_length(Length) when Length=< 0 ->
	<<0:8>>;
pack_length(Length) when Length=< 2#1111111 ->
	<<0:1,Length:7>>;
pack_length(Length) when Length=< 2#11111111111111 ->
	Byte0 = (Length band 2#1111111),
	Byte1 = (Length band 2#11111110000000) bsr 7 ,
	<<1:1,Byte0:7, 0:1,Byte1:7>>;
pack_length(Length) when Length=< 2#111111111111111111111 ->
	Byte0 = (Length band 2#1111111),
	Byte1 = (Length band 2#11111110000000) bsr 7 ,
	Byte2 = (Length band 2#111111100000000000000) bsr 14 ,
	<<1:1,Byte0:7, 1:1,Byte1:7,0:1,Byte2:7>>;
pack_length(Length) when Length=< 2#1111111111111111111111111111 ->
	Byte0 = (Length band 2#1111111),
	Byte1 = (Length band 2#11111110000000) bsr 7 ,
	Byte2 = (Length band 2#111111100000000000000) bsr 14 ,
	Byte3 = (Length band 2#1111111000000000000000000000) bsr 21 ,
	<<1:1,Byte0:7, 1:1,Byte1:7,1:1,Byte2:7,0:1,Byte3:7>>;
pack_length(_Length)->
	error.
