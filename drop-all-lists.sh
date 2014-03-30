#!/bin/sh

list_lists -b | xargs -n1 sudo list rmlist -a
