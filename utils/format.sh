#!/usr/bin/env bash

set -ue

clang-format -i $(find . -type f -regextype posix-egrep -regex ".*\.(h|hpp|c|cc|cpp)$")
