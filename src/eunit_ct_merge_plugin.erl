-module(eunit_ct_merge_plugin).
-export([merge_cover/2]).
-record(merge_info,{sum,cover}).

merge_cover(_Config,_AppFile)->
%    rebar_log:log(debug,"start merge ct and eunit cover html"),
    {ok,Cwd}=file:get_cwd(),
    BaseDir=rebar_config:get_global(base_dir,undefiend),
    CtPath=filename:join([BaseDir,"logs","*/*/*/cover.html"]),
    EunitPath=filename:join([BaseDir,".eunit"]),
    MergePath=filename:join([BaseDir,".merge"]),
    SrcPath=filename:join([BaseDir,"src"]),
    io:format("~s~n",[BaseDir]),
    case filelib:wildcard(CtPath) of
	[]->
	    %rebar_log:log(warn,"not find commont test cover.html,maybe ct not runed")
	    {error,"no common test result found,run ct first pleas!"};
	Files->
	    File=lists:last(Files),
	    Dir=filename:dirname(File),
	    case filelib:is_dir(MergePath) of
		false->
		    file:make_dir(MergePath);
		true->
		    do_nothing
	    end,
	    case filelib:is_dir(EunitPath) of
		false->
		   % rebar_log:log(error,"not find commont test cover.html,maybe ct not runed")
			ok;
		_->
		    Merges=fold_src(SrcPath,Dir,EunitPath,MergePath),
		   
		    cover_analyse(filename:basename(Cwd),MergePath,Merges),
		    ok
	    end
    end.


fold_src(_Src,Ct,Eunit,Merge)->
    filelib:fold_files(Ct,".COVER.html",false,
		       fun(FileName,Acc)->
			       FCt=filename:join([Ct,filename:basename(FileName)]),
			       FEunit=filename:join([Eunit,filename:basename(FileName)]),
			       FMerge=filename:join([Merge,filename:basename(FileName)]),
						%io:format("~s ~n ~s ~n ~s ~n",[FCt,FEunit,FMerge]),
			       T=merge_file(FCt,FEunit,FMerge),
			       [{filename:basename(FileName,".erl"),filename:basename(FMerge),T}|Acc] end,[]).



merge_file(Src1,Src2,Out)->
   R= {file:open(Src1,read),file:open(Src2,read)},

    {ok,D3}=file:open(Out,[write]),
    put(merge_info,#merge_info{sum=0,cover=0}),
    case R of
	{{ok,D1},{ok,D2}}->
	    merge_file1(file:read_line(D1),D1,D2,D3),
	    get(merge_info);
	Other->
	    io:format("merge file ~s, error reason:~p~n",[Src1,Other]),
	    #merge_info{sum=0,cover=0}
    end
    

.

merge_file1(eof,D1,D2,D3)->
    file:close(D1),
    file:close(D2),
    file:close(D3);
merge_file1({error,Reason},D1,D2,D3) ->
    error_logger:info_msg("read file  error: ~p",[Reason]),
    merge_file1(file:read_line(D1),D1,D2,D3);
merge_file1({ok,Data},D1,D2,D3) ->

    {ok,Data2}=file:read_line(D2),
    case merge_line(Data,Data2) of
	{cover,S,Count}->
	    
	    #merge_info{sum=Sum,cover=Cover}=get(merge_info),
	    Sum1=Sum+1,
	    Cover1=case Count of
		       0->
			   Cover;
		       _->Cover+1
		   end,
	    put(merge_info,#merge_info{sum=Sum1,cover=Cover1}),
	    file:write(D3,S),merge_file1(file:read_line(D1),D1,D2,D3);
	S2 ->
	    file:write(D3,S2)
		,merge_file1(file:read_line(D1),D1,D2,D3)
    end.

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
    A1=" 1..|    ?MODULE:now() + element(3, erlang:now()) / 1000000.</font>",
    B1=" 0..|    ?MODULE:now() + element(3, erlang:now()) / 1000000.</font>",
    C1=merge_line(A1,B1),
    ?assertEqual(C1,{cover," 1..|    ?MODULE:now() + element(3, erlang:now()) / 1000000.</font>",1}),
    ok.

merge_line4_test()->
    A1="2..|?MODULE:now() + element(3, erlang:now()) / 1000000.</font>",
    B1="1..|?MODULE:now() + element(3, erlang:now()) / 1000000.</font>",
    C1=merge_line(A1,B1),
    ?assertEqual(C1,{cover,"3..|?MODULE:now() + element(3, erlang:now()) / 1000000.</font>",3}),
    ok.

merge_line5_test()->
    A1="     1..|      A1=&lt;font color=red&gt;     0..|    ?MODULE:now() + element(3, erlang:now()) / 1000000.&lt;/font&gt;,",
    B1="<font color=red>     0..|      A1=\"&lt;font color=red&gt;     0..|    ?MODULE:now() + element(3, erlang:now()) / 1000000.&lt;/font&gt;\",</font>",
    C1=merge_line(B1,A1),
    ?assertEqual(C1,{cover,A1,1}),
    ok.

%

merge_file_test()->
    F1=code:lib_dir(eunit_ct_merge_plugin,'test/eunit/e1.html'),
    F2=code:lib_dir(eunit_ct_merge_plugin,'test/ct/ct1.html'),
    F3=code:lib_dir(eunit_ct_merge_plugin,'test/merge/m1.html'),
    merge_file(F1,F2,F3).

-endif.
-define(coverlog_name,"cover.html").


%%copy from common test: test_server_ctrl.erl
%% Support functions for writing the cover logs (both cross and normal)

cover_analyse(App,TestDir,Merges) ->
    
    {ok,CoverLog} = file:open(filename:join(TestDir, ?coverlog_name), [write]),
    write_coverlog_header(CoverLog),
    io:fwrite(CoverLog, "<h1>Coverage for application '~s'</h1>\n", [App]),
    %io:fwrite(CoverLog,
    %	      "<p><a href=\"~s\">Coverdata collected over all tests</a></p>",
    %	      ["merge ct and eunit" ]),
    io:fwrite(CoverLog,"<table border=3 cellpadding=5>",[]),
    io:fwrite(CoverLog,io_lib:fwrite("<th>Module</th>"
		  "<th align=right>Covered(%)</th>"
		  "<th align=right>Covered (Lines)</th>"
		  "<th align=right>Not covered(Lines)</th>",
		  []),[]),
    {Total,TCover}=lists:foldl(fun({Mod,File,{merge_info,Sum,Cover}},{All,C1})->
			io:fwrite(CoverLog,format_analyse(Mod,File,Cover,Sum-Cover),[]),
			{All+Sum,C1+Cover}
		end ,
		{0,0},Merges),
    io:fwrite(CoverLog,io_lib:fwrite("<tr><td>Total</td>"
		  "<td align=right>~w %</td>"
		  "<td align=right>~w</td>"
		  "<td align=right>~w</td></tr>",
		  [pc(TCover,Total-TCover),TCover,Total-TCover]),[]),
    io:fwrite(CoverLog,"</table>",[]),


    ok.

   %  TotPercent = write_cover_result_table(CoverLog, Coverage).
   % file:write_file(filename:join(TestDir, ),
   %		    term_to_binary(TotPercent)).

write_coverlog_header(CoverLog) ->
    case catch
	io:fwrite(CoverLog,
		  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n"
		  "<!-- autogenerated by '~w'. -->\n"
		  "<html>\n"
		  "<head><title>Coverage results</title></head>\n"
		  "<body bgcolor=\"white\" text=\"black\" "
		  "link=\"blue\" vlink=\"purple\" alink=\"red\">",
		  [?MODULE]) of
	{'EXIT',Reason} ->
	    io:format("\n\nERROR: Could not write normal heading in coverlog.\n"
		      "CoverLog: ~w\n"
		      "Reason: ~p\n",
		      [CoverLog,Reason]),
	    io:format(CoverLog,"<html><body>\n", []);
	_ ->
	    ok
    end.


format_analyse(M,FileName,Cov,NotCov) ->
    
    io_lib:fwrite("<tr><td><a href=\"~s\">~s</a></td>"
		  "<td align=right>~w %</td>"
		  "<td align=right>~w</td>"
		  "<td align=right>~w</td></tr>\n",
		  [FileName,M,pc(Cov,NotCov),Cov,NotCov]).



pc(0,0) ->
    0;
pc(Cov,NotCov) ->
    round(Cov/(Cov+NotCov)*100).

merge_line(A,B)->
    case re:run(A,"[^\\.]*([0-9])+\\.\\..*",[]) of
	{match,[_,{S1,E1}]}->
	    T=string:substr(A,S1+1,E1),
	    C1=erlang:list_to_integer(T),
	    %io:format("dddd ~p ~n ~p ~n ~p~n",[A,B,re:run(B,".*([0-9])+\\.\\..*",[])]),
	    case re:run(B,"[^\\.]*([0-9])+\\.\\..*",[]) of
		{match,[_,{S2,E2}]}->
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
		_Tt ->
		    %io:format("~p~n",[Tt]),
		    {cover,A,C1}
	    end;
	_Tt->
	    
	    A
    end
.

