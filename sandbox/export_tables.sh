#! /bin/bash
#
# show_heads.sh
# Copyright (C) 2020 Uwe Schmitt <uwe.schmitt@id.ethz.ch>
#
# Distributed under terms of the MIT license.
#

DB=PPDB_ETH_20-05-12.mdb

IFS=$'\n'

for T in $(mdb-tables -1 ${DB}); do
    echo ${T}
    mdb-export ${DB} ${T} > ${T}.csv
    echo
done
