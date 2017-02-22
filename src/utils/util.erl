-module(util).

-include_lib("xmerl/include/xmerl.hrl").

-export([to_atom/1,
		 to_integer/1,
		 to_float/1,
		 to_list/1,
		 to_binary/1,
		 list_to_string/1,
		 string_to_term/1,
		 bitstring_to_term/1,
		 term_to_string/1,
		 get_pos_in_list/2,
		 xml2tuplelist/1,
		 get_random_list_from_max/3,
		 shuffle/1,
		 rand/2,
		 get_random_list/2,
		 get_random_list_by_count/3,
		 random_string1/1,
		 random_string2/1,
		 random_by_weighting/1,
		 sleep/1]).
-export([tc/4]).

%% @doc convert other type to atom
to_atom(Msg) when is_atom(Msg) -> 
	Msg;    
to_atom(Msg) when is_binary(Msg) -> 
	list_to_atom2(binary_to_list(Msg));
to_atom(Msg) when is_list(Msg) -> 
    list_to_atom2(Msg);
to_atom(_) -> 
    throw(other_value).  %%list_to_atom("").

%% @doc convert other type to integer
%% -spec to_integer(Msg :: any()) -> integer().
to_integer(Msg) when is_integer(Msg) -> 
    Msg;
to_integer(Msg) when is_binary(Msg) ->
	Msg2 = binary_to_list(Msg),
    list_to_integer(Msg2);
to_integer(Msg) when is_list(Msg) -> 
    list_to_integer(Msg);
to_integer(Msg) when is_float(Msg) -> 
    round(Msg);
to_integer(Msg) ->
    Msg.

to_binary(Msg) when is_list(Msg)->
	list_to_binary(Msg);
to_binary(Msg) when is_integer(Msg) ->
	integer_to_binary(Msg);
to_binary(Msg) when is_float(Msg) ->
	float_to_binary(Msg);
to_binary(Msg) when is_tuple(Msg) ->
	list_to_binary(tuple_to_list(Msg));
to_binary(Msg) when is_atom(Msg) ->
	atom_to_binary(Msg, utf8);
to_binary(Msg) ->
	Msg.
	
	
to_float(Msg) when erlang:is_list(Msg) ->
	if Msg == [] ->
		   0;
	   true ->
		   list_to_float(Msg)
	end;
to_float(Msg) ->
	Msg.


list_to_atom2(List) when is_list(List) ->
	case catch(list_to_existing_atom(List)) of
		{'EXIT', _} -> erlang:list_to_atom(List);
		Atom when is_atom(Atom) -> Atom
	end.

list_to_string(List) ->
	case List == [] orelse List == "" of
		true -> "";
		false ->
			F = fun(E) ->
						to_list(E)++","
				end,
			L1 = [F(E)||E <- List] ,
			L2 = lists:concat(L1),
			string:substr(L2,1,length(L2)-1)
	end.

to_list(Msg) when is_list(Msg) -> 
    Msg;
to_list(Msg) when is_atom(Msg) -> 
    atom_to_list(Msg);
to_list(Msg) when is_binary(Msg) -> 
    binary_to_list(Msg);
to_list(Msg) when is_integer(Msg) -> 
    integer_to_list(Msg);
to_list(Msg) when is_float(Msg) -> 
    f2s(Msg);
to_list(_) ->
    throw(other_value).

f2s(N) when is_integer(N) ->
    integer_to_list(N) ++ ".00";
f2s(F) when is_float(F) ->
    [A] = io_lib:format("~.2f", [F]),
	A.

string_to_term(String) ->
	case String of
		[] -> [];
		_ ->
			case erl_scan:string(String++".") of
				{ok, Tokens, _} ->
					case erl_parse:parse_term(Tokens) of
						{ok, Term} -> Term;
						_Err -> undefined
					end;
				_Error ->
					undefined
			end
	end.

bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
    string_to_term(to_list(BitString)).


term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%%return:0/Pos in List
get_pos_in_list(Elem,List)->
	get_pos_in_list(Elem,List,1).
get_pos_in_list(_Elem,[],_Index)->
	0;
get_pos_in_list(Elem,[Elem|_],Index)->
	Index;
get_pos_in_list(Elem,[_|Left],Index)->
	get_pos_in_list(Elem,Left,Index+1).



xml2tuplelist(XmlString) ->
    Pos = string:str(XmlString,"?>"),
	XmlString2 = 
			if Pos > 0 ->
				   string:sub_string(XmlString, Pos + 2);
			   true ->
				   XmlString
			end,
    {XmlElement, _Rest} = xmerl_scan:string(XmlString2),
    Fun = fun(Atom, Ret) ->
        XPath = "//" ++ atom_to_list(Atom),
        AtomList = xmerl_xpath:string(XPath, XmlElement),
        [parse(AtomList, Atom, []) | Ret]
    end,
    TupleList = lists:flatten(lists:foldl(Fun, [], [returnstatus])),
	TupleList.
 
parse([], _Atom, Ret) ->
    Ret;
parse([XmlElement | X], Atom, Ret) ->
    case is_record(XmlElement, xmlElement) of
        true ->
            XmlText = hd(XmlElement#xmlElement.content),
            parse(X, Atom, [{Atom, XmlText#xmlText.value} | Ret]);
        false ->
            parse(X, Atom, Ret)
    end.


%%随机抽取不大于MaxNun个数的数字列表
get_random_list_from_max(MaxNum, Count, _RandomList) when Count > MaxNum ->
    [];
get_random_list_from_max(_MaxNum, 0, RandomList) ->
    RandomList;
get_random_list_from_max(MaxNum, Count, RandomList) ->
	RandNum = random:uniform(MaxNum),
	case lists:member(RandNum, RandomList) of
		true ->
			get_random_list_from_max(MaxNum, Count, RandomList);
		false ->
			get_random_list_from_max(MaxNum, Count-1, [RandNum|RandomList])
	end.
	
%% 打乱数组顺序
shuffle(List) -> shuffle(List, []).
shuffle([], Acc) -> Acc;
shuffle(List, Acc) -> 
	{Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
	shuffle(Leading ++ T, [H | Acc]).
	
%% 产生一个介于Min到Max之间的随机整数
rand(Same, Same) -> Same;
rand(Min, Max) ->
    M = Min - 1,
	if
		Max - M =< 0 ->
			0;
		true ->
    		random:seed(now()),
    	 	RanNum = random:uniform(Max - M) + M,
 		RanNum
	end.

%%随机从集合中选出指定个数的元素length(List) >= Num
%%[1,2,3,4,5,6,7,8,9]中选出三个不同的数字[1,2,4]
get_random_list(List,Num) ->
	ListSize = length(List),
	F = fun(N,List1) ->
				Random = rand(1,(ListSize-N+1)),
				Elem = lists:nth(Random, List1),
				List2 = lists:delete(Elem, List1),
				List2
		end,
	Result = lists:foldl(F, List, lists:seq(1, Num)),
	List -- Result.

get_random_list_by_count(Count, FromNum, ToNum) ->
	get_random_list_by_count2(0, Count, FromNum, ToNum, []).

get_random_list_by_count2(Max, Max, _FromNum, _ToNum, RandomList) ->
	RandomList;
get_random_list_by_count2(Count, Max, FromNum, ToNum, RandomList) ->
	RandomNum = rand(FromNum, ToNum),
	case lists:member(RandomNum, RandomList) of
		false ->
			get_random_list_by_count2(Count + 1, Max, FromNum, ToNum, [RandomNum|RandomList]);
		true ->
			get_random_list_by_count2(Count, Max, FromNum, ToNum, RandomList)
	end.

random_string1(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).

random_string2(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).

%%result: Term/[]
%%RandomList:[{Term,Weighting}]
random_by_weighting(RandomList)->
	FullWeight = lists:foldl(fun(ItemWight,LastWight)->
						LastWight +element(2,ItemWight)
				end, 0, RandomList),
	if
		FullWeight =< 0->
			[];
		true->
			RandomV = random:uniform(FullWeight),
			{Value,_} = lists:foldl(fun({Term,WightTmp},{LastValue,LastWight})->
						if
							LastValue =/= []->
								{LastValue,0};
							true->
								if
									LastWight+WightTmp >= RandomV->			%%binggo
										{Term,0};
									true->
										{[],LastWight+WightTmp}
								end
						end
				end, {[],0}, RandomList),
			Value
	end.


tc(M, F, A, N) when N > 0 ->  
    L = test_loop(M, F, A, N, []),  
    Len = length(L),  
    LSorted = lists:sort(L),  
    Min = lists:nth(1, LSorted),  
    Max = lists:nth(Len, LSorted),  
    Med = lists:nth(round(Len/2), LSorted),  
    Avg = round(lists:foldl(fun(X, Sum) ->  
                    X + Sum end,  
                0,  
                LSorted)/Len),  
    io:format("Range:~b - ~b mics~n"  
          "Median:~b mics ~n"  
          "Average:~b mics ~n",  
          [Min, Max, Med, Avg]),  
    Med.  
  
test_loop(_M, _F, _A, 0, List) ->  
    List;  
test_loop(M, F, A, N, List) ->  
    {T, _R} = timer:tc(M, F, A),  
    test_loop(M, F, A, N-1, [T|List]).  

sleep(MicroSec)-> 
   receive after MicroSec -> ok end. 
