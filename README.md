mandelbrot
==========

A simple Mandelbrot renderer for a workshop on Haskell.

To ensure you have the necessary libraries built beforehand:

```shell
stack init
stack build --profile
stack clean
stack build
```

To run, use `stack exec mandelbrot`.  The PNM data is written directly
to stdout, so redirect the output to a file:

```shell
stack exec mandelbrot > mandelbrot.pnm
```

Use `--` to stop parsing arguments to Stack and begin sending arguments
to the program:

```shell
stack exec mandelbrot -- --mode grey -w 640 -h 480
```

To enable `X` threads, pass `-NX` to the `--rts-options` argument:

```shell
stack exec --rts-options -N4 mandelbrot
```


Profiling
---------

To build with profiling, pass the `--profile` flag.  You may need to
clean first to get the project to rebuild:

```shell
stack clean
stack build --profile
```

To get a profile of the cost centres of your program, run with the `-p`
RTS option:

```shell
stack exec --rts-options -p mandelbrot
```

This will produce a file `mandelbrot.prof` with information about the
time and memory cost of each of the functions in the program.

To get a heap profile, run with the `-h` RTS option.  By default, this
will break up the heap by cost centre, but you can also display by type
(`hy`) or by constructor (`hd`):

```shell
stack exec --rts-options -hy mandelbrot
```

This will produce a file `mandelbrot.hp`.  Your Stack installation
should have come with a program `hp2ps`, which can transform this file
into the PostScript format:

```shell
hp2ps mandelbrot.hp
```

Open `mandelbrot.ps` and examine the graph of the heap.


Licensing
---------

Copyright (C) 2017 Timothy Jones

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see <http://www.gnu.org/licenses/gpl>.
