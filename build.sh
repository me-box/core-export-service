#! /bin/sh

# compile code
echo "compiling..."
eval `opam config env`
cd src && jbuilder build service.exe
cp ./_build/default/service.exe ../
echo "done compiling"
