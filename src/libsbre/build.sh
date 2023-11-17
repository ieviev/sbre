#!/usr/bin/env bash

set -euo pipefail
__SOURCE_DIRECTORY__=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

dotnet publish /p:NativeLib=Shared -c Release -r linux-x64 -o lib