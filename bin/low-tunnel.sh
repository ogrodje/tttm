#!/usr/bin/env bash
set -ex

ssh -vv -N -T -L \
	8118:0.0.0.0:8118 low cat -
