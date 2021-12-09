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