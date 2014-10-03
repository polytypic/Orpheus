#!/bin/bash

set -e

nuget restore Orpheus.sln -Verbosity quiet

function build () {
    xbuild /nologo /verbosity:quiet /p:Configuration=$1
}

build Debug
build Release
