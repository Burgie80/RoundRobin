% ------------------------
% Nombre: load_balancer.erl
% Descripción: Implementa el balanceador de carga como un proceso gen_server
%	       con la funcionalidad personalizada para registrar trabajadores
%	       y asignar tareas dinámicamente
% Equipo: Badillo Cruz Ferran
%	  López Machado Oscar Roberto 
%	  Morales Calvo Ángel Omar
%	  Galvan Godinez Antonio de Jesus
% ------------------------

-module(load_balancer).
-behaviour(gen_server).

%% API
-export([start_link/1, add_task/1]).

%% Callbacks
%%-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

-export([init/1, handle_cast/2]).

%% Función API para iniciar el balanceador de carga con una lista de trabajadores
start_link(Workers) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Workers, []).

%% Inicialización del estado (lista de trabajadores e índice inicial)
init(Workers) ->
    {ok, {Workers, 0}}.

%% Función API para agregar una tarea al balanceador
add_task(Task) ->
    gen_server:cast(?MODULE, {add_task, Task}).

%% Manejo de las tareas: asignación a los trabajadores según round-robin
handle_cast({add_task, Task}, {Workers, Index}) ->
    Worker = lists:nth(Index + 1, Workers),  %% Selecciona el trabajador según el índice
    WorkerName = "Worker" ++ integer_to_list(Index + 1),  %% Asigna un nombre al trabajador según su índice
    Worker ! {process_task, Task, WorkerName},            %% Envía la tarea al trabajador seleccionado
    NextIndex = (Index + 1) rem length(Workers),  %% Actualiza el índice para la siguiente tarea
    {noreply, {Workers, NextIndex}}.

