# author: ablatt

# The driver script for the playback parser

# imports
import sys # used for gathering the command line arguments
import utility_functions_playback_parser as utils
import estimation_functions as est
import playback_write_out as write_out

#-----------------------------------------------------------
# Relevant static data for use throughout the rest of the program
#-----------------------------------------------------------
file_types = ["vehicle_trajectory", "signal", "map", "detector", "output"]
time_step_length_in_milliseconds = 100

#-----------------------------------------------------------
# Relevant dynamic data
#-----------------------------------------------------------
signal_list = []
detector_list = []

#-----------------------------------------------------------
# Main script which drives the program
#-----------------------------------------------------------
args = sys.argv[1:] # get the actual arguments from the command line

# parse command line arguments to ensure they are files and those files do exist on the disk
write_out.init_output()
file_locations = utils.get_file_locations(args, file_types)
del args
utils.check_exists(file_locations)

if "vehicle_trajectory" not in file_locations:
	print "vehicle trajectories are necessary to create a playback simulation"
	utils.clean_and_exit()

# call utility parsing functions
for key in file_locations:
	if key == "vehicle_trajectory":
		utils.parse_vehicle_trajectory(file_locations[key])
	elif key == "signal":
		signal_list = utils.parse_signal(file_locations[key])
	elif key == "map":
		utils.parse_map(file_locations[key])
	elif key == "detector":
		detector_list = utils.parse_detector(file_locations[key])

# call estimation functions
if "map" not in file_locations:
	pass #est.estimate_map_data()

if "detector" not in file_locations:
	pass #detector_list = est.estimate_detector_data()

if "signal" not in file_locations:
	signal_list = est.estimate_signal_data()

# write out extra data
write_out.write_signal_list(signal_list, time_step_length_in_milliseconds)
write_out.write_extra_data(time_step_length_in_milliseconds)
#write_out.write_detector_list(detector_list)
# clean and zip files for the user
utils.clean_files_for_zip()
utils.zip_files()
print "SUCCESS!!!!"
