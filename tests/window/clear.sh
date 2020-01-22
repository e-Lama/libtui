#!/bin/bash

decision=''

until [[ $decision =~ ^[YyNn]$ ]]
do
    read -p "Are you sure? It will remove all .dbf, .ntx, .dbt, and .cnf files (y/n): " decision
done



if [ $decision = y ] || [ $decision = Y ]
then
    rm *.dbf *.ntx *.dbt *.cnf
fi
