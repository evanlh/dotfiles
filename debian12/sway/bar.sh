#!/bin/bash
while true
do
  date=$(date +'%Y-%m-%d %I:%M:%S %p')
  load=$(cut -d' ' -f 1,2,3 /proc/loadavg)
  msg="💻 $load 🗓️ $date "
  echo $msg
  sleep 5
done
