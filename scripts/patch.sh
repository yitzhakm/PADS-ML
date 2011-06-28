#!/bin/bash
# This scripts must be called from the pads/ml home directory.
# PADS/C home directory must be provided as first argument.

pads=$1
padsc_version=1-03

echo PADS/C version is $padsc_version.

echo Patching PADS lib.
cp scripts/patch-padsc-$padsc_version/pads.c $pads/padsc/libpads

echo Patching PADS header.
cp scripts/patch-padsc-$padsc_version/pads.h $pads/padsc/include

echo Patching PADS makefile.
cp scripts/patch-padsc-$padsc_version/rules.mk $pads/mk

echo Upgrading AST installation script.
cp scripts/patch-padsc-$padsc_version/package.cvs $pads/ast-ast/bin

echo Upgrading OS detection scripts.
cp scripts/patch-padsc-$padsc_version/*opsys* $pads/scripts
