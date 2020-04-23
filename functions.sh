#	Function for displaying help
display_help ()
{
	echo "Script for atomating wrfcamx runs"
	echo "Center for Atmospheric Particle Studies"
	echo "Carnegie Mellon University"
	echo "Syntax:"
	echo "	-d YYYY MM DD o --date YYYY MM DD"
	echo "		Required. Start date"
	echo "	-b o --debug"
	echo "		Optional. Run step by step"
	echo "		Default: FALSE"
	echo "	-s o --scenario"
	echo "		Optional. wrfcamx configuration scenario"
	echo "			Associated to a wrfcamx.in template"
	echo "		Default: 12EUS"
	echo "	-f o --forecast"
	echo "		Optional. Number of days to run"
	echo "		Default: 1"
	echo "	-h o -help"
	echo "		Optional. Displays this help text"
	exit
}
get_date ()
{
# 	Function for producing a date component vector
# 	The output variable ${fvdate[@]} contains the following elements
#		[0] = Year in YY format
#		[1] = Month in MM format
#		[2] = Day in DD format
#		[3] = Day of the year in JJJ format
#		[4] = Year in YYYY format

#	Variable capture
#	year
	local ftyear=$1
#	month
	local ftmonth=$2
#	day
	local ftday=$3
#	offset
	local fddays=$4

#	Seconds from the epoch
	local ftepoch=`date --date=$ftyear"-"$ftmonth"-"$ftday +%s`
#	Offseted seconds from the epoch
	local fepoch=$(($ftepoch+$fddays*86400))

#	Date
	local fdate=`date --date="1970-01-01 "$fepoch" sec" +"%y-%m-%d-%j-%Y"`

#	Date string parsing
#	The fvdate variable is global since its used to return data from the function
	fvdate=(`echo $fdate | tr '-' '\n'`)
#		${fvdate[0]}		Year in YY format
#		${fvdate[1]}		Month in MM format
#		${fvdate[2]}		Day in DD format
#		${fvdate[3]}		Day of the year in JJJ format
#		${fvdate[4]}		Year in YYYY format
}

pause_message ()
{
#	Function for displaying a state message and allow for continuing or terminating
	local fpause=$1
	local msg=$2
	if [ "$pause" = "-debug" ]; then
		echo $msg
		read -p "Continue (y/n)"
		if [ "$REPLY" != "y" ]; then
			echo "Stopped"
			exit
		fi
	fi
}