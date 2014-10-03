#!/bin/bash

set -e

nuget restore JSONs.sln -Verbosity quiet

function build () {
    xbuild /nologo /verbosity:quiet /p:Configuration=$1
}

build Debug
build Release
