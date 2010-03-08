#!/bin/sh
#
# Modelling access control lists and role-based access control
# in Soutei
# See the Soutei paper, section `Soutei by example'

SOUTEI_SERVER=../../soutei-server
SOUTEI_CLI=../../soutei-cli	# A sample client

HOST=localhost
PORT=1500			# Communication port with clients

echo "Starting the soutei server"
$SOUTEI_SERVER system local $PORT &

echo "server started: $!"
sleep 1				# Wait until it starts up

echo "Running sample queries"

# Peter, programmer, can read
$SOUTEI_CLI $HOST:$PORT \
 -- may read \
 -- resource TPS-report-memo \
 -- resource-owner TPS-report-owner \
 -- public-key "rsa:Z2FuZ3N0YQ==" \
 && echo granted

# Only managers can write
$SOUTEI_CLI $HOST:$PORT \
 -- may write \
 -- resource TPS-report-memo \
 -- resource-owner TPS-report-owner \
 -- public-key "rsa:Z2FuZ3N0YQ==" \
 || echo denied

# Bill is the manager
$SOUTEI_CLI $HOST:$PORT \
 -- may write \
 -- resource TPS-report-memo \
 -- resource-owner TPS-report-owner \
 -- public-key "rsa:eWVhaCBoaQ==" \
 && echo granted

# The expected sequence is `granted, denied, granted'

echo "Terminating the soutei server"
killall soutei-server

wait
echo "Done"

