#!/usr/bin/env sh

killall -q polybar
while pgrep -x polybar >/dev/null; do sleep 1; done
INTERFACE=wlp1s0 polybar --reload example &
