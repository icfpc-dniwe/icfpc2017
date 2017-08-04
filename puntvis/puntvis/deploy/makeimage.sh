#!/bin/bash

ARGC=("$#")

if [ $ARGC -eq 0 ]; then
    echo "Usage: $0 <logfile>"
    exit 1
fi

INPUT=$1
echo "input: $INPUT"

PUNTVIS="bin/puntvis.exe"
IMAGES_DIR="$INPUT-images"
GRAPHVIZ_DIR="$INPUT-graphviz"
DOT="dot"
FMT=eps
DPI=250

echo "reading log..."
"$PUNTVIS" "$INPUT"

if [ -d "$IMAGES_DIR" ]; then
    echo "removing $IMAGES_DIR"
    rm -r "$IMAGES_DIR"
fi
mkdir "$IMAGES_DIR"

echo "generating images..."
for i in "$GRAPHVIZ_DIR"/*; do
    dir=`dirname "$i"`
    base=`basename "$i"`
    "$DOT" -Gdpi="$DPI" -T$FMT "$i" -o "$IMAGES_DIR/$base.$FMT"
done