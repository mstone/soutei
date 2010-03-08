#!/bin/sh
#
# Modelling access control lists and role-based access control
# in Soutei
# See the Soutei paper, section `Soutei by example'

SOUTEI_SERVER=../../soutei-server
SOUTEI_CLI=../../soutei-cli	# A sample client

PORT=1500			# Communication port with clients

echo "Starting the soutei server"
$SOUTEI_SERVER system local $PORT &

echo "server started: $!"

echo "Running sample queries"

$SOUTEI_CLI localhost:$PORT \
 -- may read \
 -- resource TPS-report-memo \
 -- resource-owner TPS-report-owner \
 -- public-key "rsa:Z2FuZ3N0YQ==" \
 && echo granted

echo "Terminating the soutei server"
killall soutei-server

wait
echo "Done"

