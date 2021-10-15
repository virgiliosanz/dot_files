#!/bin/sh

# npm i -g icsorg
ORG_DIR=~/CloudStation/Org
curl -o $ORG_DIR/calendar.ics https://outlook.office365.com/owa/calendar/d764fb1c7931496ebdc7c7a9ba8ebe36@proofpoint.com/0206779f81614574878ada27d7a434b12128713365026166480/calendar.ics
icsorg
