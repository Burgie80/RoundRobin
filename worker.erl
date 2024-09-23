% ------------------------
% Nombre: worker.erl
% Descripción: Implementa un proceso trabajador el cual ejecuta un ciclo que continuamente espera mensajes (tareas) y simula procesarlos
% Equipo: Badillo Cruz Ferran
%         López Machado Oscar Roberto
%         Morales Calvo Ángel Omar
%         Galvan Godinez Antonio de Jesus
% ------------------------

-module(worker).
-export([start/0, loop/0]).

%% Inicia un nuevo proceso trabajador
start() ->
    spawn(fun loop/0).

%% Bucle principal del trabajador: espera y procesa tareas
loop() ->
    receive
        {process_task, Task, WorkerName} ->               %% Recibe una tarea
            io:format("~p procesando tarea: ~p~n", [WorkerName, Task]),  %% Procesa la tarea (imprime en consola)
            loop();                            %% Continúa en el bucle esperando más tareas
        _ ->                                   %% Manejo de mensajes desconocidos
            io:format("Mensaje desconocido recibido~n"),
            loop()
    end.
