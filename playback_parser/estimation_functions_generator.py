# author: ablatt

# this file contains functions for estimating data which was not immediately provided

# a wrapper function for estimating the map data
# this calls out to the estimate_map_data_functions file
# the map data estimation was moved into a separate file due to its size
def estimate_map_data():
	import estimate_map_data_functions
	estimate_map_data_functions.estimate_map_data_function()

# a function for estimating the detector data
# returns a list of all detector locations which were used to generate data
def estimate_detector_data(detectors):
	import playback_write_out as write_out
	import playback_data_model as structs
	
	#detectors = get_detectors()
	first_step, last_step = write_out.get_min_and_max_steps()
	
	print map(lambda x: "(" + str(x.bottom_left_x) + ", " + str(x.bottom_left_y) + ") (" + str(x.top_right_x) + ", " + str(x.top_right_y) + ")", detectors)
	
	count = 0
	
	for i in range(first_step, last_step + 1):
		step = write_out.repopulate_step(i)
		
		for obj in step:
			if isinstance(obj, structs.Vehicle_Trajectory_Information):
				for detector in detectors:
					if over_detector(detector, obj):
						count += 1
						detected = structs.Detector_State()
						detected.detector_number = detector.detector_number
						detected.frame_id = i
						detected.vehicle_velocity_in_miles_per_hour = obj.speed_in_miles_per_hour
						detected.vehicle_length_in_feet = obj.vehicle_length_in_feet
						detected.vehicle_width_in_feet = obj.vehicle_width_in_feet
						write_out.write_detector_data(detected)
						break
	
	print "total number of write outs = " + str(count)
	
	return detectors

# a function for estimating signal data
# this function is currently unimplemented
def estimate_signal_data():
	pass

# a function which retrieves all detectors
# returns the estimated locations of the detectors to use
def get_detectors():
	print "get all the detectors"
	
	import playback_write_out as write_out
	
	
	first_step, last_step = write_out.get_min_and_max_steps()
	lane_ids = write_out.get_lane_ids(first_step, last_step)
	
	return filter(lambda x: not x == None, map(get_detectors_from_lane_id, lane_ids))

# a function which returns true if the specified vehicle is over the specified detector
def over_detector(detector, vehicle):
	if detector.bottom_left_x <= vehicle.x_offset_in_feet and detector.bottom_left_y <= vehicle.y_offset_in_feet and detector.top_right_x >= vehicle.x_offset_in_feet and detector.top_right_y >= vehicle.y_offset_in_feet:
		return True
	
	return False

# estimates the location of the detector for a given lane
def get_detectors_from_lane_id(lane_id):
	import playback_write_out as write_out
	import playback_data_model as structs
	
	lane_centers_and_widths = write_out.get_ordered_lane_centers_and_widths(lane_id)
	print lane_centers_and_widths
	center = lane_centers_and_widths[-1][0:2]
	width = lane_centers_and_widths[-1][2]
	
	ret = structs.Detector_Internal()
	ret.detector_number = lane_id
	ret.bottom_left_x = center[0] - 5.0 # (width / 2.0)
	ret.bottom_left_y = center[1] - 5.0
	ret.top_right_x = center[0] + 5.0 # (width / 2.0)
	ret.top_right_y = center[1] + 5.0
	
	return ret

# transforms a frame and a reference point into a list which can be easily used to calculate the distance between the frame and the reference point
# note: the frame is a vehicle trajectory
def make_dist_list(frame, x, y):
	ret = []
	for f in frame:
		ret.append([frame.x_offset_in_feet, frame.y_offset_in_feet, x, y])
	
	return ret

# a utility function which calculates the distance between 2 points which are expressed in a list
# this function is meant to take in a list output by make_dist_list
def calc_dist(dist_list):
	import estimate_map_data_functions as util_funcs
	return util_funcs.get_dist(dist_list[0], dist_list[1], dist_list[2], dist_list[3])


