#!/bin/bash
str=`amixer sget Master`
str1=${str#Simple*\[}
v1=${str1%%]*]}
v2=${v1%?} # Volume without % sign

str2=${str#Simple*\[*[*[}
onoff=${str2%%]}

if [[ $onoff == "on" ]]; then
  echo "<fc=#00CC00>$v2</fc>%"
else
  echo "<fc=red>Mute</fc>"
fi
