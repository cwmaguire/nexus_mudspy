-module(mudspy_parse).

-export([parse/1]).

-define(IAC, 255). %% Interpret As Command
-define(WONT, 252).  %%
-define(WILL, 251).  %%
-define(SB, 250).  %% Start subnegotiation
-define(SE, 240).  %% End subnegotiation
-define(IRE, 201). %% Iron Realms Entertainment custom subprotocol
-define(ESC, 27).  %% Telnet escape code, \e or ^[

parse(Bin) ->
    parse(Bin, []).

parse(Bin, Acc) ->
    parse(<<>>, Bin, Acc).

%          #{gmcp => [],
%            elements => [],
%            unknown_telnet => [],
%            fgcolor => default}).

% [4z<VERSION>
% [4z<SUPPORT>
% [4z<!ELEMENT RNum ATT="id" FLAG="RoomNum" EMPTY>
% [4z<!ELEMENT RName FLAG="RoomName">
% [4z<!ELEMENT RDesc FLAG="RoomDesc">
% [4z<!ELEMENT RExits FLAG="RoomExit">
% [4z<!ELEMENT Prompt FLAG="Prompt">

%% Cases:
%% - no dangling plain text and no bytes left
%% - some dangling plain text left but no other bytes
%% - start of a GMCP message
%% - start of telnet ANSI escape code

parse(<<>>, <<>>, Acc) ->
    lists:reverse(Acc);
parse(Rest, <<>>, Acc) ->
    lists:reverse(maybe_add_text(Rest, Acc));
parse(Text, <<"\r\n", Rest/binary>>, Acc) ->
    parse(Rest, maybe_add_text(Text, Acc));
parse(Text, <<?IAC, ?WILL, ?IRE, Rest/binary>>, Acc) ->
    parse(Rest, maybe_add_text(Text, Acc));
parse(Text, <<?IAC, ?WONT, Feature:1/binary, Rest/binary>>, Acc) ->
    io:format("Won't support feature ~p~n", [Feature]),
    parse(Rest, maybe_add_text(Text, Acc));
parse(Text, <<?IAC, ?SB, Feature:1/binary, Value:1/binary, ?IAC, ?SE, Rest/binary>>, Acc) ->
    io:format("Set feature ~p to ~p~n", [Feature, Value]),
    parse(Rest, maybe_add_text(Text, Acc));
parse(Text, <<?IAC, ?SB, ?IRE, Rest/binary>>, Acc0) ->
    {Bin, Acc} = parse_gmcp(<<>>, Rest, maybe_add_text(Text, Acc0)),
    parse(Bin, Acc);
parse(Text, <<?ESC, $[, Rest/binary>>, Acc0) ->
    {Bin, Acc} = parse_telnet(Rest, maybe_add_text(Text, Acc0)),
    parse(Bin, Acc);
parse(Text, <<Byte:1/binary, Rest/binary>>, Acc) ->
    parse(<<Text/binary, Byte/binary>>, Rest, Acc).

parse_gmcp(Gmcp, <<?IAC, ?SE, Rest/binary>>, Acc) ->
    {Rest, [{gmcp, Gmcp} | Acc]};
parse_gmcp(Gmcp, <<>>, Acc) ->
    {<<>>, [{gmcp, Gmcp} | Acc]};
parse_gmcp(Gmcp, <<Byte:1/binary, Rest/binary>>, Acc) ->
    parse_gmcp(<<Gmcp/binary, Byte/binary>>, Rest, Acc).

parse_telnet(<<"0m", Rest/binary>> Acc) ->
    {Rest, [{fgcolor, <<"reset">>} | Acc]};
parse_telnet(<<"4z", Rest/binary>>, Acc) ->
    parse_custom_telnet(Rest, Acc);
parse_telnet(<<"38;5;0", Rest/binary>>, Acc) ->
    parse_telnet_foreground_color(Rest, Acc).

parse_custom_telnet(<<"<VERSION>", Rest/binary>>, Acc) ->
    {Rest, [{version, <<"version">>} | Acc]};
parse_custom_telnet(<<"<SUPPORT>", Rest/binary>>, Acc) ->
    {Rest, [{support, <<"support">>} | Acc]};
parse_custom_telnet(<<"<!ELEMENT ", Rest/binary>>, Acc0) ->
    parse_custom_element(<<>>, Rest, Acc0);
parse_custom_telnet(<<"<", Rest/binary>>, Acc0) ->
    parse_custom_element(<<>>, Rest, Acc0).

parse_telnet_foreground_color(<<ColorCode:2/binary, "m", Rest/binary>>, Acc) ->
    Color = case ColorCode of
                <<"00">> -> <<"black">>;
                <<"01">> -> <<"red">>;
                <<"02">> -> <<"green">>;
                <<"03">> -> <<"yellow">>;
                <<"04">> -> <<"blue">>;
                <<"05">> -> <<"purple">>;
                <<"06">> -> <<"teal">>;
                <<"07">> -> <<"grey">>;
                <<"08">> -> <<"bright_grey">>;
                <<"09">> -> <<"bright_red">>;
                <<"10">> -> <<"bright_green">>;
                <<"11">> -> <<"bright_yellow">>;
                <<"12">> -> <<"bright_blue">>;
                <<"13">> -> <<"bright_purple">>;
                <<"14">> -> <<"bright_teal">>;
                <<"15">> -> <<"white">>
            end,
    {Rest, [{fgcolor, Color} | Acc]}.

parse_custom_element(Element, <<$/, $>, Rest/binary>>, Acc) ->
    {Rest, [{element, Element} | Acc]};
parse_custom_element(Element, <<$>, Rest/binary>>, Acc) ->
    {Rest, [{element, Element} | Acc]};
parse_custom_element(Element, <<Byte:1/binary, Rest/binary>>, Acc) ->
    parse_custom_element(<<Element/binary, Byte/binary>>, Rest, Acc).

maybe_add_text(<<>>, Acc) ->
    Acc;
maybe_add_text(Text, Acc) ->
    [{text, Text} | Acc].

% [38;5;007m[38;5;015m*** You have new unread news ***
%
% [38;5;007m
% ÿúÉChar.Defences.Add
% { "name": "boartattoo", "desc": "This tattoo will passively regenerate your health." }
% ÿð
% The vitality of the black boar begins to flow through you.
%
%ÿúÉ
% Char.Defences.Add
% { "name": "mosstattoo", "desc": "This tattoo will passively clot your bleeding." }
% ÿð
% Your moss tattoo tingles slightly.
%
% [38;5;004m     (Blue?)
% *------------------(
% [38;5;007m     (Grey?)
% [38;5;015m     (White?)
% Announcements from the Administration
% [38;5;007m
% [38;5;004m
% )
% ------------------*
%
%[38;5;007m
%[38;5;015m
%-&amp;gt; Congratulations to Imyrr and Ashtan for winning the Staff of Nicator!
%
%[38;5;007m
%[38;5;015m
%-&amp;gt; Compete for the Effigy of Victory in the CTF on Saturday 19th!
%
%[38;5;007m
%[4z&lt;RNum 530 /&gt;
%[4z&lt;RName&gt;
%[38;5;003m
%Shellfish market.
%[38;5;007m
%[4z&lt;/RName&gt;
