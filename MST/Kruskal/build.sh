#!/bin/sh

stack build diagrams diagrams-builder
stack exec -- pdflatex --enable-write18 Kruskal.tex
stack exec -- pdflatex --enable-write18 Kruskal-key.tex
