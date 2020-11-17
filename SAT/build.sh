#!/bin/sh

stack build diagrams diagrams-builder
stack exec -- rubber -d --unsafe *.tex
