# RoundRobin
Implementación de un balanceador de carga simple utilizando la estrategia round-robin escrito en Erlang.

## Requisitos
Erlang/OTP instalado en tu sistema.

## Archivos
`load_balancer.erl`: Implementa el balanceador de carga.\
`worker.erl`: Define el comportamiento de los trabajadores.\
`startup.erl`: Permite iniciar el sistema con trabajadores y agregar tareas.\

## Códigos
### load_balancer.erl

```erlang
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
```

### worker.erl

```erlang
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
```

### startup.erl

```erlang
% ------------------------
% Nombre: startup.erl
% Descripción: Implementa un proceso cliente el cual inicia el balanceador con
%	       una lista de trabajadores predefinida, y asigna tareas dinámicamente
% Equipo: Badillo Cruz Ferran
%         López Machado Oscar Roberto
%         Morales Calvo Ángel Omar
%         Galvan Godinez Antonio de Jesus
% ------------------------

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
```

## Cómo ejecutar
### Compilar los módulos
En la terminal de Erlang, compila los archivos .erl:

```erlang
c(load_balancer).
c(worker).
c(startup).
````

### Iniciar el sistema
Ejecuta la función `startup:start_system/0` para inicializar el sistema de balanceo de carga con 3 trabajadores y algunas tareas:

```erlang
startup:start_system().
```

Esto lanzará el balanceador de carga y asignará tareas a los trabajadores de manera automática.

### Ejemplo de interacción
Al ejecutar `startup:start_system/0`, el sistema asignará las tareas task1, task2, task3, etc., a los trabajadores. La salida en la consola debería verse así:

```erlang
"Worker1" procesando tarea: task1
"Worker2" procesando tarea: task2
"Worker3" procesando tarea: task3
"Worker1" procesando tarea: task4
"Worker2" procesando tarea: task5
```

Cada trabajador procesa las tareas en orden round-robin, con su nombre asignado de forma dinámica por el balanceador.

## Asciinema
[![asciicast](https://asciinema.org/a/677460.svg)](https://asciinema.org/a/677460)
