#!/usr/bin/env sh

set -e

stack build :gradient-design

stack exec -- gradient-design
