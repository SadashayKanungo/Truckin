% Definitions of the world
:- [board].
:- [world]. 

% The ref and the agents.
:- [solo_ref].

:- multifile
	move/3,
	init_agent/1.

settings( agent1, greedy_agent1). % Set the name of agent1. Ensure that if your agent's name is xyz, the agent should be defined in a file called xyz.prolog  % EDIT TO USE OTHER AGENT
settings( agent2, my_agent). % Uncomment this line to set the name of the second agent for two player case.
settings( moves, 30 ).		% Set the # of moves you want the game to last for.

run_game:-
	randomize_board, % Uncomment this line to test your agent on a randomly generated board
	ref_reset_dynamic,
	settings(agent1, Agent1),
	settings(agent2, Agent2), %Uncomment line 19,22,25 and set the agent name in line 12 if you wish to test two agents against each other.  
	settings(moves, TotalMoves),
	consult(Agent1),
	consult(Agent2),
	asserta(moves_remaining(TotalMoves)),
	start_atruck(Agent1),
	start_atruck(Agent2),
	start_ref,!,
	writeln('\n\n---FINISHED---\n\n').