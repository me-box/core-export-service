databox-bridge — bridge component for databox
-------------------------------------------------------------------------------
%%VERSION%%

databox-bridge is TODO

databox-bridge is distributed under the ISC license.

Homepage: https://github.com/sevenEng/databox-bridge  

## Installation

databox-bridge can be installed with `opam`:

    opam install databox-bridge

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
databox-bridge`.

[doc]: https://www.cl.cam.ac.uk/~ql272/databox-bridge/doc

## Sample programs

If you installed databox-bridge with `opam` sample programs are located in
the directory `opam var databox-bridge:doc`.

In the distribution sample programs and tests are located in the
[`test`](test) directory of the distribution. They can be built and run
with:

    topkg build --tests true && topkg test 
