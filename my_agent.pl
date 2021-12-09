:- dynamic
	agent_choice2/3,
	agent_moves/1,
	agent_state2/1,
	moves_remaining/1,
	place/6,
	player/6,
	holding/3.

%:- [my_helper].
%:- [my_master].
agent_name(my_agent).

init_agent(my_agent):-
	asserta(agent_state2(init)).




%Init
move(my_agent,_,_):-
	agent_state2(init),
	writeln('INITIALIZING AGENT'),
	retractall( agent_choice2(_,_,_) ),
	find_best_trio( MySeller, MyBuyer, MyFueler ),
	asserta( agent_choice2(MySeller, MyBuyer, MyFueler) ),
	writeln(agent_choice2(MySeller, MyBuyer, MyFueler)),
	get_best_sequence(MySeq),
	MySeq = seq(Moves,_,_,_,_,_),
	reverse(Moves, RevMoves),
	[start|MyMoves] = RevMoves,
	retractall( agent_moves(_) ),
	asserta( agent_moves(MyMoves) ),
	writeln( agent_moves(RevMoves) ),
	set_agent_state2(decide),
	fail.
	
%Decide next stop
move(my_agent,_,_):-
	agent_state2(decide),
	agent_moves(MyMoves),
	[NextStop | LaterMoves] = MyMoves,
	retract(agent_moves(MyMoves)),
	asserta(agent_moves(LaterMoves)),
	writeln(agent_state(NextStop)),
	set_agent_state2(NextStop),
	fail.

% Go to Finish
move(my_agent, MoveType, MoveQuantity):-
	agent_state2(finish),
	moves_remaining(Mrem),
	Mrem<5,
	writeln('MOVE TO FINISH'),
	MoveType = m,
	place(FinishIndex,_,_,_,_,finish),
	step_to_go_to2( FinishIndex, MoveQuantity).

% Finish repeated to call init when MoveQuantity becomes zero due to greedy agent's moves
move(my_agent, MoveType, MoveQuantity):-
	agent_state2(finish),
	moves_remaining(Mrem),
	Mrem>4,
	writeln('STARTING NEW INITIALIZATION, MOVE LOST'),
	set_agent_state2(init),
	fail.

% Refuel
move(my_agent, MoveType, MoveQuantity):-
	agent_state2(fuel),
	player(my_agent,_,_,_,Fuel,Where),
	agent_choice2(_,_,MyFueler),
	Where = MyFueler,
	writeln('REFUELING'),
	MoveType = t,
	moves_remaining(MRem),
	fuel_quantity(29-MRem, Fuel, MoveQuantity),
	set_agent_state2(decide).
% Go to Fuel
move(my_agent, MoveType, MoveQuantity):-
	agent_state2(fuel),
	player(my_agent,_,_,_,Fuel,Where),
	agent_choice2(_,_,MyFueler),
	Where \= MyFueler,
	writeln('MOVE TO FUEL'),
	MoveType = m,
	step_to_go_to2(MyFueler,MoveQuantity).

% Make Purchase
move(my_agent, MoveType, MoveQuantity):-
	agent_state2(buy),
	player(my_agent,_,Cash,_,_,Where),
	agent_choice2(MySeller,_,_),
	Where = MySeller,
	writeln('BUYING'),
	MoveType = t,
	max_purchase(Cash,MoveQuantity),
	MoveQuantity>0,
	set_agent_state2(decide).

% Go to Buy
move(my_agent, MoveType, MoveQuantity):-
	agent_state2(buy),
	player(my_agent,_,Cash,_,_,Where),
	agent_choice2(MySeller,_,_),
	Where \= MySeller,
	writeln('MOVE TO BUY'),
	MoveType = m,
	step_to_go_to2(MySeller,MoveQuantity).

% Make Sale
move(my_agent, MoveType, MoveQuantity):-
	agent_state2(sell),
	player(my_agent,_,_,_,_,Where),
	agent_choice2(_, MyBuyer,_),
	Where = MyBuyer,
	writeln('SELLING'),
	MoveType = t,
	holding(my_agent, _, MoveQuantity),
	MoveQuantity>0,
	set_agent_state2(decide).

% Go to Sell
move(my_agent, MoveType, MoveQuantity):-
	agent_state2(sell),
	player(my_agent,_,_,_,_,Where),
	agent_choice2(_, MyBuyer,_),
	Where \= MyBuyer,
	writeln('MOVE TO SELL'),
	MoveType = m,
	step_to_go_to2(MyBuyer, MoveQuantity).

% Make Purchase failed due to greedy agent's moves
move(my_agent, _, _):-
	agent_state2(buy),
	player(my_agent,_,Cash,_,_,Where),
	agent_choice2(MySeller,_,_),
	Where = MySeller,
	writeln('COULD NOT BUY, MOVE LOST'),
	set_agent_state2(init).
% Make Sale failed due to greedy agent's moves
move(my_agent, _, _):-
	agent_state2(sell),
	player(my_agent,_,_,_,_,Where),
	agent_choice2(_, MyBuyer,_),
	Where = MyBuyer,
	writeln('COULD NOT SELL, MOVE LOST'),
	set_agent_state2(init).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MY HELPER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	get_dist(P1,P2,D):-
	D is min(abs(P1-P2), 64-abs(P1-P2)).


set_agent_state2(ToState):-
	retractall(agent_state2(_)),
	asserta( agent_state2(ToState) ).

step_to_go_to2( Dest, 0 ):-
	player(my_agent,_,_,_,_, Where ),
	Where = Dest.

step_to_go_to2(Dest, MoveQuantity):-
	player(my_agent,_,_,_,_, Where ),
	Diff is ((Dest-Where+64) mod 64),
	Diff < 32,
	MoveQuantity is min(8, Diff).

step_to_go_to2( Dest, MoveQuantity):-
	% Returns how many steps to move to go home
	player(my_agent,_,_,_,_, Where ),
	Diff is ((Dest-Where+64) mod 64),
	Diff >= 32,
	MoveQuantity is max(-8, Diff-64).

max_purchase(Cash,ShipQuantity):-
	agent_choice2(MySeller, MyBuyer,_),
	place(MySeller,_,Item,SQ,Price,seller),
	SQ>0,
	place(MyBuyer,_,Item,BQ,_,buyer),
	BQ>0,
	player(my_agent, WL, _, VL, _, _),
	item( Item, IW, IV),
	WLim is floor(WL/IW),
	VLim is floor(VL/IV),
	CLim is floor(Cash/Price),
	min_list([WLim,VLim, CLim],OurLim),
	min_list([SQ, BQ, floor(OurLim)], ShipQuantity).

find_best_trio(MySeller, MyBuyer, MyFueler):-
	% Finds dealers with best ratio of how much profit we obtain on buying and selling Q of the item to how much fuel is required to get it. Here Q is the minimum of how much the buyer wants, how much the seller can sell and how much we can buy based on how much cash, weight and volume we have.
	player(my_agent, Weight, Cash, Volume, Fuel, Where),
	writeln(player(my_agent, Weight, Cash, Volume, Fuel, Where)),
	findall(
		sbf(Place1,Place2,Place3,Score), % Just a name for the structure
		(
				place(Place1,_,Item,Quantity1,Price1,seller),
				place(Place2,_,Item,Quantity2,Price2,buyer),
				place(Place3,_,'Fuel',Quantity3,Price3,seller),
				Quantity1>0,
				Quantity2>0,
				Quantity3>0,
				item( Item, IW, IV ),
				WLim is floor(Weight/IW),
				VLim is floor(Volume/IV),
				CLim is floor(Cash/Price1),
				min_list([WLim,VLim, CLim],Quantity),
				Quantity>0,
				get_dist(Place1,Place2,Dist12),
				get_dist(Place1,Place3,Dist13),
				get_dist(Place2,Place3,Dist23),
				moves_remaining(Mrem),
				CycleProfit is (Quantity * (Price2 - Price1)) - (Price3 * Dist12),
				NominalCycles is min( min(50/Dist12, Mrem/(2*ceiling(Dist12/8))), min(Quantity1,Quantity2)/Quantity),
				CycleFuelerCost is ((Price3 * (Dist13+Dist23)) / (min(Quantity1,Quantity2)/Quantity)),
				Score is (NominalCycles * (CycleProfit - CycleFuelerCost))
				% writeln(sbf(Place1,Place2,Place3,Score)),
				% writeln(NominalProfit),
				% writeln(FuelerScore)
		),
		SBFTrios
	),
	find_max_score(SBFTrios, MaxScore),
	member(sbf(MySeller, MyBuyer, MyFueler, MaxScore), SBFTrios),
	write('BEST TRIO : '),
	writeln(sbf(MySeller, MyBuyer, MyFueler, MaxScore)).

find_max_score(SBFTrios, MaxScore):-
	% Returns the member of SBFTrios having max Score
	findall( Score, member(sbf(_,_,_,Score), SBFTrios) , ScoreList ),
	max_list(ScoreList, Max),
	MaxScore is Max.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MY MASTER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	:- dynamic
    found_seqs/1.

get_loc(buy,L):-
    agent_choice2(L,_,_).
get_loc(sell,L):-
    agent_choice2(_,L,_).
get_loc(fuel,L):-
    agent_choice2(_,_,L).
get_loc(start,L):-
    player(my_agent, _, _, _, _, L).
get_loc(finish,L):-
    place(L,_,_,_,_,finish).

get_fuel(P1,P2,F):-
    get_loc(P1,L1),
    get_loc(P2,L2),
    get_dist(L1,L2,F).

get_moves(P1,P2,M):-
    get_loc(P1,L1),
    get_loc(P2,L2),
    get_dist(L1,L2,D),
    M is ceil(D/8).

max_transaction(MT):-
    agent_choice2(MySeller, MyBuyer,_),
	place(MySeller,_,_,SQ,_,seller),
    place(MyBuyer,_,_,BQ,_,buyer),
    MT is min(SQ,BQ).

shipment_quantity(Cash,Trans,ShipQuantity):-
	agent_choice2(MySeller, MyBuyer,_),
	place(MySeller,_,Item,SQ,Price,seller),
	SQ>0,
	place(MyBuyer,_,Item,BQ,_,buyer),
	BQ>0,
	player(my_agent, WL, _, VL, _, _),
	item( Item, IW, IV),
	WLim is floor(WL/IW),
	VLim is floor(VL/IV),
	CLim is floor(Cash/Price),
	min_list([WLim,VLim, CLim],OurLim),
	min_list([SQ-Trans, BQ-Trans, floor(OurLim)], ShipQuantity1),
    ShipQuantity is max(ShipQuantity1,1).

shipment_cost_price(Price):-
    agent_choice2(MySeller,_,_),
    place(MySeller,_,_,_,Price,seller).

shipment_sale_price(Price):-
    agent_choice2(_,MyBuyer,_),
    place(MyBuyer,_,_,_,Price,buyer).

fuel_quantity(MoveNos,Fuel,Q):-
    moves_remaining(Mrem),
    Q is min((MoveNos)*8, (100-Fuel)).

fuel_price(FP):-
    agent_choice2(_,_,MyFueler),
    place(MyFueler,_,_,_,FP,seller).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sequence structure is seq(Moves[],MoveNos,Bal,Fuel,HoldingQ,TransactionQ)

add_buy(Seq,NewSeq):-
    %writeln('add buy'),
    Seq = seq(Moves,MoveNos,Bal,Fuel,Hold,Trans),
    [At | _] = Moves,
    NewMoves = [buy | Moves],
    get_moves(At,buy,M),
    NewMoveNos is MoveNos - (M + 1),
    shipment_cost_price(SC),
    shipment_quantity(Bal,Trans,SQ),
    NewBal is Bal - (SC*SQ),
    NewHold is Hold + SQ,
    NewTrans is Trans + SQ,
    get_fuel(At,buy, F),
    NewFuel is Fuel - F,
    NewSeq = seq(NewMoves, NewMoveNos, NewBal, NewFuel, NewHold, NewTrans).
    %writeln(NewSeq).

add_sell(Seq,NewSeq):-
    %writeln('add sell'),
    Seq = seq(Moves,MoveNos,Bal,Fuel,Hold, Trans),
    [At | _] = Moves,
    NewMoves = [sell | Moves],
    get_moves(At,sell,M),
    NewMoveNos is MoveNos - (M + 1),
    shipment_sale_price(SC),
    NewBal is Bal + (SC*Hold),
    NewHold = 0,
    get_fuel(At,sell, F),
    NewFuel is Fuel - F,
    NewSeq = seq(NewMoves, NewMoveNos, NewBal, NewFuel, NewHold, Trans).
    %writeln(NewSeq).


add_fuel(Seq,NewSeq):-
    %writeln('add fuel'),
    Seq = seq(Moves,MoveNos,Bal,Fuel,Hold,Trans),
    [At | _] = Moves,
    NewMoves = [fuel | Moves],
    get_moves(At,fuel,M),
    NewMoveNos is MoveNos - (M + 1),
    NewHold = Hold,
    get_fuel(At,fuel, F),
    fuel_price(FP),
    fuel_quantity(NewMoveNos,Fuel-F,FQ),
    FuelCost is FP * FQ,
    NewFuel is Fuel - F + FQ,
    NewBal is Bal - FuelCost,
    NewSeq = seq(NewMoves, NewMoveNos, NewBal, NewFuel, NewHold,Trans).
    % (At = start , writeln('DEBUG'),writeln(NewSeq),writeln(FuelCost),writeln(FQ),writeln(FP),writeln('END DEBUG') ; true).
    %writeln(NewSeq).

add_finish(Seq,NewSeq):-
    %writeln('add finish'),
    Seq = seq(Moves,MoveNos,Bal,Fuel,Hold,Trans),
    [At | _] = Moves,
    NewMoves = [finish | Moves],
    get_moves(At,finish,M),
    NewMoveNos is MoveNos - M,
    NewBal = Bal,
    NewHold = Hold,
    get_fuel(At,finish, F),
    NewFuel is Fuel - F,
    NewSeq = seq(NewMoves, NewMoveNos, NewBal, NewFuel, NewHold,Trans).
    %writeln(NewSeq).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
found_seqs([]).

% from finish
make_seq(Seq):-
    %writeln('Initialization may take a few seconds, Do not terminate...'),
    Seq = seq(Moves,MoveNos,_,_,_,_),
    MoveNos >= 0,
    [LastMove | _] = Moves,
    LastMove = finish,
    found_seqs(Found),
    NewFound = [Seq | Found],
    %writeln(Found),
    %writeln(NewFound),
    retract(found_seqs(_)),
    assert(found_seqs(NewFound)),
    fail.
% from buy
make_seq(Seq):-
    %writeln('Initialization may take a few seconds, Do not terminate...'),
    Seq = seq(Moves,MoveNos,_,_,_,_),
    MoveNos > 0,
    [LastMove | _] = Moves,
    LastMove = buy,
    add_sell(Seq,SellSeq),
    add_fuel(Seq,FuelSeq),
    add_finish(Seq,FinishSeq),
    SellSeq = seq(_,_,SellBal,SellFuel,_,_),
    FuelSeq = seq(_,_,FuelBal,FuelFuel,_,_),
    FinishSeq = seq(_,_,FinishBal,FinishFuel,_,_),
    ((SellBal>0,SellFuel>0,make_seq(SellSeq)) ; true),
    ((FuelBal>0,FuelFuel>0,make_seq(FuelSeq)) ; true),
    ((FinishBal>0,FinishFuel>0,make_seq(FinishSeq)) ; true),
    fail.
    
% from sell
make_seq(Seq):-
    %writeln('Initialization may take a few seconds, Do not terminate.'),
    Seq = seq(Moves,MoveNos,_,_,_,_),
    MoveNos > 0,
    [LastMove | _] = Moves,
    LastMove = sell,
    add_buy(Seq,BuySeq),
    add_fuel(Seq,FuelSeq),
    add_finish(Seq,FinishSeq),
    BuySeq = seq(_,_,BuyBal,BuyFuel,_,T),
    FuelSeq = seq(_,_,FuelBal,FuelFuel,_,_),
    FinishSeq = seq(_,_,FinishBal,FinishFuel,_,_),
    max_transaction(MT),
    ((BuyBal>0,BuyFuel>0,T=<MT,make_seq(BuySeq)) ; true),
    ((FuelBal>0,FuelFuel>0,make_seq(FuelSeq)) ; true),
    ((FinishBal>0,FinishFuel>0,make_seq(FinishSeq)) ; true),
    fail.
    
% from fuel
make_seq(Seq):-
    %writeln('Initialization may take a few seconds, Do not terminate....'),
    Seq = seq(Moves,MoveNos,_,_,_,_),
    MoveNos > 0,
    [LastMove | _] = Moves,
    LastMove = fuel,
    add_buy(Seq,BuySeq),
    add_sell(Seq,SellSeq),
    add_finish(Seq,FinishSeq),
    max_transaction(MT),
    SellSeq = seq(_,_,SellBal,SellFuel,Hs,_),
    BuySeq = seq(_,_,BuyBal,BuyFuel,Hb,T),
    FinishSeq = seq(_,_,FinishBal,FinishFuel,_,_),
    ((SellBal>0,SellFuel>0,Hs>0,make_seq(SellSeq)) ; true),
    ((BuyBal>0,BuyFuel>0,T=<MT,Hb=0,make_seq(BuySeq)) ; true),
    ((FinishBal>0,FinishFuel>0,make_seq(FinishSeq)) ; true),
    fail.
    
% from start
make_seq(Seq):-
    Seq = seq(_,MoveNos,_,_,_,_),
    moves_remaining(Mrem),
    MoveNos = Mrem,
    add_buy(Seq,BuySeq),
    add_fuel(Seq,FuelSeq),
    add_finish(Seq,FinishSeq),
    max_transaction(MT),
    BuySeq = seq(_,_,BuyBal,BuyFuel,_,T),
    FuelSeq = seq(_,_,FuelBal,FuelFuel,_,_),
    FinishSeq = seq(_,_,FinishBal,FinishFuel,_,_),
    ((BuyBal>0,BuyFuel>0,T=<MT,make_seq(BuySeq)) ; true),
    ((FuelBal>0,FuelFuel>0,make_seq(FuelSeq)) ; true),
    ((FinishBal>0,FinishFuel>0,make_seq(FinishSeq)) ; true),
    fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_best_seq(Seqs, BestSeq):-
	findall( Bal, member(seq(_,_,Bal,_,_,_), Seqs) , BalList ),
	max_list(BalList, Max),
	findall(Seq, (Seq=seq(_,_,Max,_,_,_), member(Seq, Seqs)), BestSeqs),
    [BestSeq|_] = BestSeqs.

get_best_sequence(BestSeq):-
    moves_remaining(Mrem),
    write('FOR MOVES REMAINING : '),
    writeln(Mrem),
    player(my_agent, _, Cash, _, Fuel, _),
    retract( found_seqs(_) ),
    asserta( found_seqs([]) ),
    writeln('THIS MAY TAKE A WHILE, DO NOT TERMINATE...'),
    (make_seq(seq([start],Mrem,Cash,Fuel,0,0)) ; true),
    found_seqs(FoundSeqs),
    %writeln(FoundSeqs),
    find_best_seq(FoundSeqs,BestSeq),
    write('BEST SEQUENCE : '),
	writeln(BestSeq).
