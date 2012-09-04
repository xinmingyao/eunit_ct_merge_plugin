-module(ect_tool).
-export([merge_line/2]).

-spec merge_line(A::string(),B::string())->{ok,C::string()}|{error,Reason::any()}.
merge_line(A,B)->
    case re:run(A,".*([0-9])+\\.\\..*",[]) of
	{match,[_,{S1,E1}]}->
	    T=string:substr(A,S1+1,E1),
	    C1=erlang:list_to_integer(T),
	    {match,[_,{S2,E2}]}=re:run(B,".*([0-9])+\\.\\..*",[]),
	    T2=string:substr(B,S2+1,E2),
	    C2=erlang:list_to_integer(T2),

	    case {C1,C2} of
		{0,0}->
		    {cover,A,0};
		{0,_} ->
		    {cover,B,C2};
		{_,0} ->
		    {cover,A,C1};
		_ ->
		    C3=C1+C2,
		    Tt=string:substr(A,1,S1)
			++erlang:integer_to_list(C3)
			++string:substr(A,S1+E1+1,length(A)-E1),
		    {cover,Tt,C3}
	    end;
	_ ->

	    A
    end
.




-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

merge_line_test()->
    A1="<font color=red>     0..|    ?MODULE:now() + element(3, erlang:now()) / 1000000.</font>",
    B1="<font color=red>     0..|    ?MODULE:now() + element(3, erlang:now()) / 1000000.</font>",
    C1=merge_line(A1,B1),
    ?assertEqual(C1,{cover,"<font color=red>     0..|    ?MODULE:now() + element(3, erlang:now()) / 1000000.</font>",0}),
    ok.
merge_line2_test()->
    A1="<font color=red>     0..|    ?MODULE:now() + element(3, erlang:now()) / 1000000.</font>",
    B1="1..|    ?MODULE:now() + element(3, erlang:now()) / 1000000.</font>",
    C1=merge_line(A1,B1),
    ?assertEqual(C1,{cover,"1..|    ?MODULE:now() + element(3, erlang:now()) / 1000000.</font>",1}),
    ok.

merge_line3_test()->
    A1="2..|?MODULE:now() + element(3, erlang:now()) / 1000000.</font>",
    B1="1..|?MODULE:now() + element(3, erlang:now()) / 1000000.</font>",
    C1=merge_line(A1,B1),
    ?assertEqual(C1,{cover,"3..|?MODULE:now() + element(3, erlang:now()) / 1000000.</font>",3}),
    ok.
    
-endif.

