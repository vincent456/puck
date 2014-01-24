#!/bin/bash

for i in $(seq 1 $1) #{1..10} 
do
    name="visual_trace$i"
    dot2png "$name.dot" #&& eog "$name.png"
done
