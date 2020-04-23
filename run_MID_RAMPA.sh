#!/bin/bash
#
# Script for atomating MID_RAMPA runs
# Grupo de Investigaciones Ambientales
# Universidad Pontificia Bolivariana
#
# Author:
#	Pablo GarcIa
#

#	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*
# Functions
source functions.sh

#	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*
#					A	R	G	U	M	E	N	T	S
#	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*

# Argument capture for type -?
if [ $# -ge 4 ] # At least 4 arguments are necessary
	then
		# Loop the arguments
		while [ $# -ge 1 ]
			do
			# Argument selector, shift offsets the arguments
			case $1 in
				-d  | --date ) tyear=$2; tmonth=$3; tday=$4; shift 4;;
				-b  | --debug ) pause="-debug"; shift;;
				-s  | --scenario ) scenario=$2; shift 2;;
				-f  | --forecast ) forecast=$2; shift 2;;
				-h  | --help ) display_help; shift;;
				*) echo "Non valid argument"; exit;;
			esac
		done
else
	display_help
fi

# Check the date, does it exist and is a number?
if [ -z $tyear ] || [ -z $tmonth ] || [ -z $tday ]
	then
	echo "Not a valid date"
	exit
fi

# Default $scenario value
if [ -z $scenario ]
	then
	scenario="brams_amva"
fi

# Default $forecast value
if [ -z $forecast ]
	then
	forecast=1
fi

#	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*
#	   				M	I	D		R	A	M	P	A
#	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*

# Loop through the forcast days
for i in $(eval echo {1..$forecast})
do
	# Today
	get_date $tyear $tmonth $tday $((i-1))
	declare -a tvdate=(${fvdate[@]})
	
	# Debug
	pause_message $pause "Update the MID_RAMPA.nml file"
	# Update the MID_RAMPA.nml file
	source MID_RAMPA.nml.$scenario

	#	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*	*
	# Debug
	pause_message $pause "Run MID_RAMPA for ${tvdate[4]} ${tvdate[1]} ${tvdate[2]}"
	# Run ramxcamx
	./MID_RAMPA | tee MID_RAMPA.$scenario.log
	if [ "$?" -ne "0" ]; then
		echo "Run failed"
		exit 1
	else
		echo "Run successful."
	fi
done
