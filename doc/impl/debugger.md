# Debugger.fnl (v0.0.3)

**Table of contents**

- [`invoke-debugger`](#invoke-debugger)

## `invoke-debugger`
Function signature:

```
(invoke-debugger condition-object scope level)
```

Invokes interactive debugger for given `condition-object`.  Accepts
`scope` with bound restarts, and optional `level`, indicating current
debugger depth.

Restarts in the menu are ordered by their definition order and dynamic
scope depth.  Restarts can be called by their number in the menu or
the name in square brackets.  For example, if `restart-case` defines
two restarts `:a` and `:b` and outer `restart-case` bounds restarts
`:a` and `:c` the following menu will be printed:

```
Debugger was invoked on unhandled condition:
1: [a    ] a
2: [b    ] b
3:         a
4: [c    ] c
5: [throw] Throw condition as a Lua error
debugger>>
```

If restart function has a docstring, it is printed after the square
brackets.  If no docstring is found, restart name is printed.

If restart accepts any arguments, a second prompt will be entered when
restart is chosen from the menu.  Above the prompt a hint with
argument names will be displayed.  In this prompt arguments to the
restart are provided as expressions separated by space:

```
Provide inputs for some-restart (args: [some-arg other-arg]) (^D to cancel)
debugger:some-restart>> (+ 1 2 3) {:some :table}
```

Debugger doesn't know anything about the environment, or variables, so
in this prompt only fully realized values can be used.

If an error happens during restart call, debug level increases, and new
`cancel` restart is added to the menu, that allows returning to
previous debug level.


---

Copyright (C) 2021 Andrey Listopadov

License: [MIT](https://gitlab.com/andreyorst/fennel-conditions/-/raw/master/LICENSE)


<!-- Generated with Fenneldoc v0.1.5
     https://gitlab.com/andreyorst/fenneldoc -->
