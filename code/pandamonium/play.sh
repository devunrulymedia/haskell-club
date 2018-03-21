#!/bin/bash

set -eio pipefail

stack test
stack exec pandamonium-exe
