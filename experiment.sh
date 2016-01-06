#!/bin/bash

while true; do
    #update source code
    git pull origin master
    #compile and build experiment.jar
    sbt assembly
    #run experiment
    java -d64 -server -XX:+AggressiveOpts -XX:+UseLargePages \
         -jar target/scala-2.10/Experiment.jar
done
