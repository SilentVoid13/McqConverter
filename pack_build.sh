#!/bin/bash

BIN_DIR=bin

x86_64_LINUX=target/release
x86_64_WINDOWS=target/x86_64-pc-windows-gnu

cp $x86_64_LINUX/amctxt2gift $BIN_DIR/x86_64_linux_amctxt2gift
cp $x86_64_LINUX/gift2amctxt $BIN_DIR/x86_64_linux_gift2amctxt
cp $x86_64_LINUX/mcqconverter_gui $BIN_DIR/x86_64_linux_mcqconverter_gui

cp $x86_64_WINDOWS/release/amctxt2gift.exe $BIN_DIR/x86_64_windows_amctxt2gift.exe
cp $x86_64_WINDOWS/release/gift2amctxt.exe $BIN_DIR/x86_64_windows_gift2amctxt.exe
cp $x86_64_WINDOWS/release/mcqconverter_gui.exe $BIN_DIR/x86_64_windows_mcqconverter_gui.exe

strip $BIN_DIR/*

#zip -r -j amc_gift_binaries.zip $BIN_DIR/*
