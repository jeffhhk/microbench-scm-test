#!/bin/bash
adirScript=$( cd $( dirname "$0" ) && pwd )
"$adirScript/chezscheme.sh" --optimize-level 3 --script app5_chez.scm

