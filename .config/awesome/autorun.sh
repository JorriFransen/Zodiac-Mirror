#! /usr/bin/env bash

function run {
    if ! pgrep $1 ;
    then
        $@&
    fi
}

run compton
run ~/.packages/arch_touchpad_setup/arch_touchpad_setup.bash
run nm-applet
run blueman-applet
run cbatticon
run pasystray
