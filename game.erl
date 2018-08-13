-module(game).
-behaviour (gen_fsm).

%% Behavior functions
-export([start/1, start_link/1]).
%% gen_fsm callbacks
-export([
    % Behaviour callbacks
    init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
    % State functions
    waiting_for_players/2, in_progress/2
]).
%% Utility functions
-export([get_number/1, add_player/2, get_active_player/1,
    shut_down/1
]).

-record(data, {
    number,
    players = [],
    active_player,
    game = {0,0,0,0,0,0,0,0,0},
    turn =0
}).

start(Number) ->
	start_link(Number).

start_link(Number) ->
	gen_fsm:start(?MODULE, [Number], []).


% Callback functions

init([Number]) ->
	{ok, waiting_for_players, #data{number = Number}}.

handle_event(stop, StateName, Data) ->
	{stop, normal, Data}.

handle_sync_event(get_number, _From, StateName, Data) ->
	{reply, Data#data.number, StateName, Data};
handle_sync_event(get_player, _From, StateName, Data) ->
	{reply, Data#data.active_player, StateName, Data}.

handle_info(Info, StateName, Data) ->
	io:format("handle_info, Info : ~p ~n", [Info]).

code_change(_OldVsn, StateName, Data, _Extra) ->
	{ok, StateName, Data}.

terminate(_Reason, _StateName, _Data) ->
  io:format("Terminating~n"),
  ok.


% State functions

waiting_for_players({add_player, Player}, Data) ->
	Game = Data#data.number,
	io:format("Game ~p adding player ~p ~n", [Game, player:get_name(Player)]),
	NewPlayers = [Player | Data#data.players],
	PlayerCount = length(NewPlayers),
	gen_fsm:send_event(Player, {game_joined, self()}),
	ActivePlayer = play(PlayerCount, NewPlayers, Data#data.game),
	case PlayerCount of
		1 ->
			{next_state, waiting_for_players, Data#data{ players = NewPlayers, active_player = ActivePlayer}};
		2 ->
			{next_state, in_progress, Data#data{ players = NewPlayers, active_player = ActivePlayer}}
	end.

in_progress({player_action, Player, {Row, Column}}, Data) ->
	Game = Data#data.game,
	io:format("in_progress, Player ~p played Row : ~p, Column: ~p, Game : ~p~n", [Player, Row, Column, Game]),
	Turn = Data#data.turn,
	Players = Data#data.players,
	Index = index(Column, Row),
	case is_valid_action(Index, Data#data.game) of
		true ->
			NewGame = setelement(Index, Game, Player),
			CheckWinner = check_winner(NewGame, Turn+1),
			in_progress_valid_action(CheckWinner, Turn+1, Player, other_player(Players,Player), NewGame, Data);	
		_ ->
			in_progress_invalid_action(Player, {Row,Column}, Data)
	end.

% Utility Functions

get_number(Game) ->
	gen_fsm:sync_send_all_state_event(Game, get_number).

add_player(Game, Player) ->
	gen_fsm:send_event(Game, {add_player, Player}).

get_active_player(Game) ->
	gen_fsm:sync_send_all_state_event(Game, get_player).

shut_down(Game) ->
	gen_fsm:send_all_state_event(Game, stop).

% Helper functions

in_progress_valid_action(par, NewTurn, Player, NewPlayer, NewGame, Data) ->
	gen_fsm:send_event(Player, {game_over, draw}),
	gen_fsm:send_event(NewPlayer, {game_over, draw}),
	{stop, normal, Data};

in_progress_valid_action(none, NewTurn, Player, NewPlayer, NewGame, Data) ->
	gen_fsm:send_event(Player, got_move),
	gen_fsm:send_event(NewPlayer, {your_move, NewGame}),
	{next_state, in_progress, Data#data{game = NewGame, turn = NewTurn, active_player = NewPlayer}};

in_progress_valid_action(Winner, _, _, _, _, Data) ->
	gen_fsm:send_event(Winner, {game_over, winner}),
	gen_fsm:send_event(other_player(Data#data.players, Winner), {game_over, loser}),
	{stop, normal, Data}.

in_progress_invalid_action(Player, Action, Data) ->
	gen_fsm:send_event(Player, {bad_action, Action}),
	{next_state, in_progress, Data}.


index(C, R) when ((C =:= 1) orelse (C =:= 2) orelse (C =:= 3)) andalso
                 ((R =:= 1) orelse (R =:= 2) orelse (R =:= 3)) ->
    C + (R - 1) * 3.

check_winner({X,X,X,_,_,_,_,_,_},_) when X /= 0 -> X;
check_winner({_,_,_,X,X,X,_,_,_},_) when X /= 0 -> X;
check_winner({_,_,_,_,_,_,X,X,X},_) when X /= 0 -> X;
check_winner({X,_,_,X,_,_,X,_,_},_) when X /= 0 -> X;
check_winner({_,X,_,_,X,_,_,X,_},_) when X /= 0 -> X;
check_winner({_,_,X,_,_,X,_,_,X},_) when X /= 0 -> X;
check_winner({X,_,_,_,X,_,_,_,X},_) when X /= 0 -> X;
check_winner({_,_,X,_,X,_,X,_,_},_) when X /= 0 -> X;
check_winner(_,9) -> par;
check_winner(_,_) -> none.

other_player([P1,P2],P1) -> P2;
other_player([P1,_],_) -> P1.

is_valid_action(I, Game) when is_integer(I) ->
    element(I, Game) == 0;
is_valid_action(_,_) -> false.

play(1, _, _) -> none;
play(2, [P1, _], Game) ->
	gen_fsm:send_event(P1, {your_move, Game}).