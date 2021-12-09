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