#!/bin/sh

set -ex
# cd "$(dirname "$0")"

pwd
exe="${TARGET-unknown_target}/primenumbers"
test -x "$exe"

exec "$exe"
