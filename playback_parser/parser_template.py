
# this file is a template to be filled in with project specific code
# the user may want to refer to peach_tree_parser.py for an example of how to fill in the below functions

# this file contains functions for parsing the peach tree data

# parse a file into a list of vehicle trajectories (class Vehicle_Trajectory_Information from the playback_data_model)
# return None if you would prefer to parse the vehicle trajectory file line by line (where 1 line = 1 vehicle trajectory)
# this alternative is preferred in the event of a large number of vehicle trajectories which must be parsed
# Note: you should use parse_vehicle_trajectory_file or parse_vehicle_trajectory but not both
def parse_vehicle_trajectory_file(f, fname):
	pass

# parse a line from fname into a vehicle trajectory
# this alternative is preferred in the event of a large number of vehicle trajectories which must be parsed
# Note: you should use parse_vehicle_trajectory_file or parse_vehicle_trajectory but not both
# this function will only be called if parse_vehicle_trajectory_file returned None
def parse_vehicle_trajectory(line, fname):
	pass

# parse a file into a list of into a list of signal timings
# Note: you should use parse_signal_file or parse_signal but not both
# this function will be called on every execution; return None if you prefer to use parse signal
def parse_signal_file(f, fname):
	pass

# a function for parsing a single line of the signal file into a Single_Lane_Signal_State
# Note: you should use parse_signal_file or parse_signal but not both
# this function will be called only when parse_signal_file returns None
def parse_signal(line, fname):
	pass
