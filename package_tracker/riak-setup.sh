#!/bin/bash
# This script is specificly for installing riak on a fresh Digital Ocean droplet running Ubuntu under a created user

# Make sure we are in the user home directory
cd $HOME

# Update repos and install package dependencies necessary to install riak from source
sudo apt update -y
sudo apt install erlang build-essential cmake libpam0g-dev -y

# Clone the github repo for basho/riak
git clone https://github.com/basho/riak.git

# Scope to the directory that was just created that contains all riak files
cd $HOME/riak

# Use rebar3 to get all erlang module dependencies
sudo ./rebar3 get-deps
# Build a release of riak using source build tools
sudo make rel

# Get the public IP address of the current machine (i.e. this droplet)
PUB_IP=$(hostname -I | cut --delimiter " " --fields 1)

# Edit the riak.conf file to use the public IP for node creation
# NOTE - not sure if this is necessary
sudo sed -i "402,411s/127.0.0.1/$PUB_IP/" $HOME/riak/rel/riak/etc/riak.conf

echo $'Install successful.\n'

echo $'To start a riak node do the following:\n\ncd ~/riak/rel/riak/bin\nsudo ./riak daemon\n'
