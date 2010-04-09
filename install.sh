#!/bin/bash

ROOT=$('pwd')
for i in *; do 
    ln -fsv $ROOT/$i ~/.$i; 
done