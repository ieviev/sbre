#!/usr/bin/env bash

__SOURCE_DIRECTORY__=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

dotnet run -c Debug "-p:DefineConstants=\"DIAGNOSTIC;DEBUG;\""
