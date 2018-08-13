-module(player).
-behaviour(gen_fsm).

-record(data, {
		user,
		game = none
	}).

-export([start/1, start_link/1]).
-export([
    % Behaviour callbacks
    init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
    % State functions
    waiting_for_game/2, in_game/2
]).
-export([
    get_name/1, quit/1, game_quit/1, make_move/3
]).

start(User) ->
	start_link(User).

start_link(User) ->
	gen_fsm:start(?MODULE, [User], []).


% Callbacks

init([User]) ->
	{ok, waiting_for_game, #data{ user = User, game = none}}.

handle_event(stop, _StateName, Data) ->
	{stop, normal, Data}.

handle_sync_event(get_name, _From, StateName, Data) ->
	{reply, Data#data.user, StateName, Data}.

handle_info(Info, StateName, Data) ->
	io:format("handle_info called, Info ~p ~n", [Info]),
	{next_state, StateName, Data}.

terminate(_Reason, _StateName, _Data) ->
  io:format("Terminating~n"),
  ok.

code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.


% State Functions

waiting_for_game({game_joined, Game}, Data) ->
	io:format("Player ~p joined the game ~p/~p ~n", [Data#data.user, game:get_number(Game), Game]),
	{next_state, in_game, Data#data{ game = Game }}.

in_game(game_quit, Data) ->
	io:format("Player ~p quit the game ~n", [Data#data.user]),
	{next_state, waiting_for_game, Data#data{ game = none}};

in_game({game_over, Winner}, Data) ->
	Result = case Winner of
		winner -> "You won !";
		loser -> "You lose !";
		draw -> "There is no winner"
	end,
	io:format("Game Over , Result : ~p ~n", [Result]),
	{next_state, waiting_for_game, Data#data{ game = none }};

in_game({your_move, Game}, Data) ->
	printPlay(Data#data.user, Game),
	{next_state, in_game, Data};

in_game({make_move, Row, Column}, Data) ->
	Game = Data#data.game,
	gen_fsm:send_event(Game, {player_action, self(), {Row, Column}}),
	{next_state, in_game, Data};

in_game(got_move, Data) ->
	io:format("Move accepted~n"),
	{next_state, in_game, Data};

in_game({bad_action, Action}, Data) ->
	io:format("Invalid move ~n"),
	{next_state, in_game, Data};

in_game(not_your_turn, Data) ->
	io:format("Not your turn, wait ~n"),
	{next_state, in_game, Data}.


% Utility functions


quit(Player) ->
	gen_fsm:send_all_state_event(Player, stop).

get_name(Player) ->
	gen_fsm:sync_send_all_state_event(Player, get_name).

game_quit(Player) ->
	gen_fsm:send_event(Player, game_quit).

make_move(Player, Row, Column) ->
	gen_fsm:send_event(Player, {make_move, Row, Column}).


% Helper functions

printPlay(User, Game) ->
	io:format("~s in_game: it is your turn to play~n~n", [User]),
    Me = self(),
    L = [sign(X,Me) || X <- tuple_to_list(Game)],
    print_game(L).

print_game([A,B,C|R]) ->
    io:format("+-+-+-+~n|~c|~c|~c|~n",[A,B,C]),
    print_game(R);
print_game([]) ->
    io:format("+-+-+-+~n~n",[]).

sign(0,_) -> 32;
sign(X,X) -> $X;
sign(_,_) -> $O.


