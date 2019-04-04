# author: ablatt

# this file holds functions for dealing with file io

# a function which initilizes the output folder
def init_output():
	import os
	import os.path
	
	if os.path.exists("playback_temp_output_folder"):
		print "There is already a folder called playback_temp_output_folder"
		print "please delete that folder so this execution can use a fresh output folder"
		exit() # do not clean up because that would delete this possibly necessary directory
	
	os.mkdir("playback_temp_output_folder")
	os.mkdir("playback_temp_output_folder" + os.sep + "static_data")
	os.mkdir("playback_temp_output_folder" + os.sep + "step_data")

#  a function which gets rid of the output directory
def clean_destroy():
	import os
	os.remove("playback_temp_output_folder")

# a function which removes unnecessary section separators at the end of step data files
def clean_step_data():
	import os
	
	fname_base = "playback_temp_output_folder" + os.sep + "step_data" + os.sep
	files = os.listdir(fname_base)
	
	for f in files:
		f = fname_base + f
		# read in the file
		g = open(f, 'r')
		file_str = g.read()
		g.close()
		
		file_str = file_str[0:-len("\n==section separator==\n")]
		
		# write the file back out
		h = open(f, 'w')
		h.write(file_str)
		h.close()

# a function which removes unnecessary section separators at the end of map data
def clean_static_data():
	import os
	
	fname_base = "playback_temp_output_folder" + os.sep + "static_data" + os.sep
	f = open(fname_base + "map_data.txt", 'r')
	file_str = f.read()
	f.close()
	
	file_str = file_str[0:-len("\n==section separator==\n")]
	
	g = open(fname_base + "map_data.txt", 'w')
	g.write(file_str)
	g.close()

# a function which writes a vehicle trajectory to its appropriate file in step data
def write_vehicle_trajectory(trajectory):
	if trajectory == None:
		return
	
	import os
	import os.path
	
	fname = "playback_temp_output_folder" + os.sep + "step_data" + os.sep + str(trajectory.frame_id) + ".txt"
	
	f = open(fname, 'a')
	
	f.write("vehicle\n") # label this section
	sub_write(f, "vehicle_id", trajectory.vehicle_id)
	sub_write(f, "global_time", trajectory.global_time)
	sub_write(f, "x_offset_in_feet", trajectory.x_offset_in_feet)
	sub_write(f, "y_offset_in_feet", trajectory.y_offset_in_feet)
	sub_write(f, "latitude", trajectory.latitude)
	sub_write(f, "longitude", trajectory.longitude)
	sub_write(f, "speed_in_miles_per_hour", trajectory.speed_in_miles_per_hour)
	sub_write(f, "acceleration_in_miles_per_hour_squared", trajectory.acceleration_in_miles_per_hour_squared)
	sub_write(f, "vehicle_length_in_feet", trajectory.vehicle_length_in_feet)
	sub_write(f, "vehicle_width_in_feet", trajectory.vehicle_width_in_feet)
	sub_write(f, "lane_id", trajectory.lane_id)
	f.write("==section separator==\n")
	
	f.close()

# a function which writes the list of signal states to their appropriate files
def write_signal_list(signal_list, time_step_length_in_milliseconds):
	import os
	print "writing out signal data"
	to_iterate = os.listdir("playback_temp_output_folder" + os.sep + "step_data")
	low = int(to_iterate[0][0:-4])
	high = int(to_iterate[0][0:-4])
	
	for name in to_iterate:
		num = int(name[0:-4])
		if num < low:
			low = num
		
		if num > high:
			high = num
	
	for i in range(len(signal_list)):
		if signal_list[i].signal_start_step == 0:
			signal_list[i].signal_start_step = low
		elif signal_list[i].signal_end_step == -1:
			signal_list[i].signal_end_step = high
	
	#for step in to_iterate:
	#	step_num = int(step[0:-4])
	for step_num in range(low, high + 1):
		
		for signal in signal_list:
			if step_num >= signal.signal_start_step and step_num <= signal.signal_end_step:
				f = open("playback_temp_output_folder" + os.sep + "step_data" + os.sep + str(step_num) + ".txt", 'a')
				
				f.write("signal\n")
				sub_write(f, "lane_id", signal.lane_id)
				sub_write(f, "signal_state", signal.current_signal_state)
				sub_write(f, "time_to_change_in_milliseconds", (signal.signal_end_step - step_num)*time_step_length_in_milliseconds)
				f.write("==section separator==\n")
				
				f.close()

# a function which writes the map data to a file
def write_map_data(map_data):
	import os
	print "writing out map data"
	f = open("playback_temp_output_folder" + os.sep + "static_data" + os.sep + "map_data.txt", 'w')
	
	sub_write(f, "reference_x", map_data.reference_x)
	sub_write(f, "reference_y", map_data.reference_y)
	f.write("==section separator==\n")
	for lane in map_data.lane_data:
		sub_write(f, "lane_id", lane.lane_id)
		sub_write(f, "is_egress", lane.is_egress)
		sub_write(f, "lane_width_in_feet", lane.lane_width_in_feet)
		sub_write(f, "lane_has_right_turn", lane.lane_has_right_turn)
		sub_write(f, "lane_has_right_turn_on_red", lane.lane_has_right_turn_on_red)
		sub_write(f, "lane_has_left_turn", lane.lane_has_left_turn)
		sub_write(f, "lane_has_left_turn_on_red", lane.lane_has_left_turn_on_red)
		sub_write(f, "lane_has_u_turn", lane.lane_has_u_turn)
		f.write("==begin center points==\n")
		
		center_point_write_out_list = []
		
		for i in range(len(lane.center_points)):
			tmp = "x_in_feet:" + str(lane.center_points[i][0]) + "\n"
			tmp += "y_in_feet:" + str(lane.center_points[i][1]) + "\n"
			tmp += "width_in_feet:" + str(lane.widths_from_center[i]) + "\n"
			center_point_write_out_list.append(tmp)
		
		for i in range(len(center_point_write_out_list) - 1):
			f.write(center_point_write_out_list[i])
			f.write("==center point divider==\n")
		
		f.write(center_point_write_out_list[-1])
		f.write("==end center points==\n")
		f.write("==begin lane connector list==\n")
		
		for connector in lane.connector_lanes:
			sub_write(f, "lane_id", connector)
		
		f.write("==end lane connector list==\n")
		f.write("==section separator==\n")
	
	f.close()

# a function which writes a detector event to its appropriate file in step data
def write_detector_data(detector):
	import os
	
	f = open("playback_temp_output_folder" + os.sep + "step_data" + os.sep + str(detector.frame_id) + ".txt", 'a')
	
	f.write("detector\n")
	sub_write(f, "detector_number", detector.detector_number)
	sub_write(f, "vehicle_velocity_in_miles_per_hour", detector.vehicle_velocity_in_miles_per_hour)
	sub_write(f, "vehicle_length_in_feet", detector.vehicle_length_in_feet)
	sub_write(f, "vehicle_width_in_feet", detector.vehicle_width_in_feet)
	f.write("==section separator==\n")
	
	f.close()

# a function which writes extra data to the miscellaneous file
def write_extra_data(time_step_length_in_milliseconds):
	import os
	import playback_data_model as structs
	
	first, last = get_min_and_max_steps()
	global_time = reduce(lambda x, y: x, filter(lambda x: isinstance(x, structs.Vehicle_Trajectory_Information), repopulate_step(first))).global_time
	
	f = open("playback_temp_output_folder" + os.sep + "static_data" + os.sep + "miscellaneous.txt", 'w')
	
	f.write("extra data\n")
	sub_write(f, "total_num_time_steps", last - first)
	sub_write(f, "first_time_step", global_time)
	sub_write(f, "time_step_length_in_milliseconds", time_step_length_in_milliseconds)
	
	f.close()

# a function which writes detector location data to the miscellaneous file
def write_detector_list(detector_list):
	import os
	
	f = open("playback_temp_output_folder" + os.sep + "static_data" + os.sep + "miscellaneous.txt", 'a')
	f.write("==section separator==\ndetector list\n" + reduce(lambda x, y: x + "==detector separator==\n" + y, map(transform_detector, detector_list)))
	f.close()

# a utility function which builds a detector into a string
def transform_detector(detector):
	ret = build_str_for_out("detector_number", detector.detector_number)
	ret += build_str_for_out("bottom_left_x", detector.bottom_left_x)
	ret += build_str_for_out("bottom_left_y", detector.bottom_left_y)
	ret += build_str_for_out("top_right_x", detector.top_right_x)
	ret += build_str_for_out("top_right_y", detector.top_right_y)
	
	return ret

# utility function which builds a key value pair into a single string
def build_str_for_out(key, val):
	return key + ":" + str(val) + "\n"

# utility function for writing out a key value pair to a file
def sub_write(f, key, val):
	f.write(build_str_for_out(key, val))

# a function which reconstructs a step into a list of states
# states can be vehicle trajectories, signal states and detector events
def repopulate_step(step_num):
	import os
	import playback_data_model as structs
	
	f = open("playback_temp_output_folder" + os.sep + "step_data" + os.sep + str(step_num) + ".txt")
	step = f.read()
	f.close()
	
	ret = []
	pieces = step.split("\n==section separator==\n")
	for piece in pieces:
		parts = piece.split("\n")
		title = parts[0]
		for i in range(1, len(parts)):
			parts[i] = parts[i].split(":")[1].strip() # get the actual element associated with the id
		
		if title == "vehicle":
			trajectory = structs.Vehicle_Trajectory_Information()
			trajectory.frame_id = step_num
			trajectory.vehicle_id = parts[1]
			trajectory.global_time = int(parts[2])
			trajectory.x_offset_in_feet = float(parts[3])
			trajectory.y_offset_in_feet = float(parts[4])
			trajectory.latitude = float(parts[5])
			trajectory.longitude = float(parts[6])
			trajectory.speed_in_miles_per_hour = float(parts[7])
			trajectory.acceleration_in_miles_per_hour_squared = float(parts[8])
			trajectory.vehicle_length_in_feet = float(parts[9])
			trajectory.vehicle_width_in_feet = float(parts[10])
			trajectory.lane_id = int(parts[11])
			ret.append(trajectory)
		elif title == "signal":
			signal = structs.Single_Lane_Signal_State()
			signal.lane_id = int(parts[1])
			signal.current_signal_state = parts[2]
			ret.append(signal)
		elif title == "detector":
			detector = structs.Detector_State()
			detector.detector_number = int(parts[1])
			detector.frame_id = step_num
			detector.vehicle_velocity_in_miles_per_hour =  float(parts[2])
			detector.vehicle_length_in_feet = float(parts[3])
			detector.vehicle_width_in_feet = float(parts[4])
			ret.append(detector)
	
	return ret

# a function which fetches and returns the first and last steps which are in the step data
def get_min_and_max_steps():
	import os
	to_iterate = os.listdir("playback_temp_output_folder" + os.sep + "step_data")
	
	low = int(to_iterate[0][0:-4])
	high = int(to_iterate[0][0:-4])
	
	for name in to_iterate:
		num = int(name[0:-4])
		if num < low:
			low = num
		
		if num > high:
			high = num
	
	return [low, high]

# a function which gets a list of all lane ids
def get_lane_ids(first_step, last_step):
	import playback_data_model as structs
	
	known_lane_ids = []
	known_vehicles = []
	for i in range(first_step, last_step + 1):
		step = repopulate_step(i)
		
		for obj in step:
			if isinstance(obj, structs.Single_Lane_Signal_State) or isinstance(obj, structs.Detector_State):
				pass
			elif isinstance(obj, structs.Vehicle_Trajectory_Information):
				if obj.lane_id not in known_lane_ids:
					known_lane_ids.append(obj.lane_id)
				if obj.vehicle_id not in known_vehicles:
					known_vehicles.append(obj.vehicle_id)
	
	print "there are " + str(len(known_vehicles)) + " unique vehicles"
	
	return known_lane_ids

# a function which returns a list of lane centers and widths of the form [[center_x, center_y, width], [center_x, center_y, width] ... ]
def get_ordered_lane_centers_and_widths(lane_id):
	import os
	f = open("playback_temp_output_folder" + os.sep + "static_data" + os.sep + "map_data.txt", 'r')
	contents = f.read()
	f.close()
	
	spl = contents.split("\n==section separator==\n")[1:-1]
	del contents
	spl = map(lambda x: x.split("\n==end center points==\n")[0], spl)
	
	for s in spl:
		parts = s.split("\n")
		if int(parts[0].split(":")[1]) == lane_id:
			print "here"
			parts = s.split("\n==begin center points==\n")[1].split("\n==center point divider==\n")
			# break the data into components and extract the values from those components
			parts = map(lambda y: map(lambda z: float(z.split(":")[1]), y), map(lambda x: x.split("\n"), parts))
			return parts
	
	return None
