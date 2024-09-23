# RoundRobin
Implementación de un balanceador de carga simple utilizando la estrategia round-robin escrito en Erlang.

## Requisitos
Erlang/OTP instalado en tu sistema.

## Archivos
`load_balancer.erl`: Implementa el balanceador de carga.\
`worker.erl`: Define el comportamiento de los trabajadores.\
`startup.erl`: Permite iniciar el sistema con trabajadores y agregar tareas.\

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
