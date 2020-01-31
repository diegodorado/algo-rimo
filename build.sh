#!/bin/sh

set -e

js="dist/elm.js"
min="dist/elm.min.js"

mkdir -p dist
rm -rf dist/*
cp index.html dist/
sed -i 's/elm\.js/elm\.min\.js/g' dist/index.html
cp favicon.ico dist/
cp -R assets dist/

#elm make --optimize --output=$js $@
elm make --optimize --output=$js src/Main.elm

uglifyjs $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=$min
rm $js

echo "Compiled size:$(cat $js | wc -c) bytes  ($js)"
echo "Minified size:$(cat $min | wc -c) bytes  ($min)"
echo "Gzipped size: $(cat $min | gzip -c | wc -c) bytes"
