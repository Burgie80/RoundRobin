-module(startup).
-export([start_system/0]).

%% Inicia el sistema con 3 trabajadores y balanceador
start_system() ->
    Worker1 = worker:start(),
    Worker2 = worker:start(),
    Worker3 = worker:start(),
    Workers = [Worker1, Worker2, Worker3],    %% Lista de trabajadores
    load_balancer:start_link(Workers),        %% Inicia el balanceador con los trabajadores

    %% Agregar tareas al balanceador
    load_balancer:add_task(task1),
    load_balancer:add_task(task2),
    load_balancer:add_task(task3),
    load_balancer:add_task(task4),
    load_balancer:add_task(task5).
