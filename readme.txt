-----------
0. Contents
-----------
	1. Introduction
	2. Files
	3. Using the human agent
	4. Writing your own agent 
	5. Notes

-----------------
1. Introduction
-----------------
Truckin' is a board game. The aim is to be the player with the most money at the end of the day. You make money by transporting goods across the board from buyers to sellers. You also have to keep an eye on the fuel you have. A detailed explanation can be found in the 'rules.txt' file.
The game is designed to be multi-player / multi-agent.

---------
2. Files
---------
- readme.txt
	= This file. Intended to be a well written guide to help you build agents for Truckin'.

- rules.txt
	= The rules file for the Truckin' game. 
	= Read this immediately after this file.

- solo_ref.prolog
	= A referee program that gives your agent information about the board and gets the move it wants to make.
	= It's ok if you don't know how this works.
	= The change will not cause any (significant) changes in your agent.

- human_agent.prolog
	= An interactive agent for you to play and familiarize yourself with the game.
	= Usage is described in the next section

- game.prolog
	= The script that initializes and starts the game.

- board.prolog
	= A file describing the board. Contains a list of places.

- world.prolog
	= Contains facts about the world such as
		* Weight, Volume of unit item
		* Capacity of trucks
		* Size of the board ( The board is a square like Monopoly. You keep going around )
		* Other rules such as starting cash, how many squares you can move in a turn ( MaxMove )

-------------------------
3. Using the human agent
-------------------------
- The human agent will output your present location and your state.
- Use this, along with the info in board.prolog and world.prolog to make your move.
- To start a game with the human agent, run game.prolog and run the query 
	?- run_game.

--------------------------
4. Writing your own agent
--------------------------
( To use your agent instead of the human agent, Edit the relevant lines in game.prolog )
Every agent is required to have the following predicates defined, as they will be called by the ref:

init_agent:-
	Starts your agent. Clear agent specific predicates and so on.


move( player, Move, Quantity ):-
	Decides the move that the agent wants to make.
	- player		Name of the player
	- Move			The kind of move to be made. Can be either of
					- m ( move )
					- t ( transact )
	- Quantity		If Move is m, 
					The number of steps to move forward.
					This must lie between -MaxMove and +MaxMove
					Where MaxMove is defined in world.prolog as max_move(MaxMove).
				If Move is t, ( Only on buyer/seller/tip )
					The quantity that you want to buy/sell.
					The maximum you can buy/sell will be limited by either 
						- the cash you have left OR
						- the amount the buyer/seller is willing to buy/sell

	Your agent can make use of the following facts to get knowledge of the world

	- player(player, weight, cash, volume, fuel, where)
		Describes the state of the player
		player		Name of the player.
		weight		Some remaining truck capacity ( in grammes ).			
		cash		Amount of money the player has ( in cents ).	
		volume		Some remaining truck capacity ( in millilitres ).		
		fuel		Some remaining fuel ( in litres ).
		where		Place index.

	- place(where, name, item, quantity, price, type )
		Describes the state of the place
		- where		Place index
		- name		Place name
		- item		The item the place buys / sells 
		- quantity	The amount of Item that the place is willing to buy/sell
		- price		The price at which it sells a unit of Item
		- type		One of ( start, finish, buyer, seller, tip, weight, hazard )
				Read rules.txt for more info

	- holding(player,item,quantity)
		Indicates `Player` is holding `Quantity` of `Item`
		- Player	Name of the player
		- Item		Item the player is holding
		- Quantity	Quantity of `Item` that the player is holding




--------
5. Notes
--------
- You will be evaluated on a two-player board. Your agent will compete with another agent who will play at the same time as you.
  You can find instructions to test your code for a two-player board, and also randomly generated boards, in game.prolog.
- There will be strict plagiarism checks for all submissions.

