# author: ablatt

# this file contains functions for parsing the peach tree data

# parse a file into a list of vehicle trajectories (class Vehicle_Trajectory_Information from the playback_data_model)
# return None if you would prefer to parse the vehicle trajectory file line by line (where 1 line = 1 vehicle trajectory)
# this alternative is preferred in the event of a large number of vehicle trajectories which must be parsed
# Note: you should use parse_vehicle_trajectory_file or parse_vehicle_trajectory but not both
def parse_vehicle_trajectory_file(f, fname):
	return None

# parse a line from fname into a vehicle trajectory
# this alternative is preferred in the event of a large number of vehicle trajectories which must be parsed
# Note: you should use parse_vehicle_trajectory_file or parse_vehicle_trajectory but not both
# this function will only be called if parse_vehicle_trajectory_file returned None
def parse_vehicle_trajectory(line, fname):
	if not fname == "trajectories-1245pm-0100pm.txt":#"trajectories-0400pm-0415pm.txt":
		return None
	
	import playback_data_model as structs
	
	spl = line.split()
	
	'''
	x_bottom_left_feet_offset = 38.0
	y_bottom_left_feet_offset = 219.0
	x_top_right_feet_offset = 54.0
	y_top_right_feet_offset = 223.0
	'''
	# for 10th street
	'''
	x_bottom_left_feet_offset = -78.2
	y_bottom_left_feet_offset = 0
	x_top_right_feet_offset = 60.2
	y_top_right_feet_offset = 685
	#'''
	# for 11th street
	'''
	x_bottom_left_feet_offset = -85.12
	y_bottom_left_feet_offset = 248.2
	x_top_right_feet_offset = 57.31
	y_top_right_feet_offset = 1195.4
	#'''
	
	# for 12th street
	#'''
	x_bottom_left_feet_offset = -63.74
	y_bottom_left_feet_offset = 787.0
	x_top_right_feet_offset = 78.10
	y_top_right_feet_offset = 1615.0
	#'''
	# get the lane id
	
	lane_id_map = {}
	''' -> 10th street
	lane_id_map["+,-,2,11"] = 1
	lane_id_map["-,-,2,11"] = 1
	lane_id_map["+,-,2,1"] = 2
	lane_id_map["-,-,2,1"] = 2
	lane_id_map["+,-,2,2"] = 3
	lane_id_map["-,-,2,2"] = 3
	lane_id_map["+,+,1,3"] = 4
	lane_id_map["+,-,1,3"] = 4
	lane_id_map["+,+,1,2"] = 5
	lane_id_map["+,-,1,2"] = 5
	lane_id_map["+,+,1,1"] = 6
	lane_id_map["+,-,1,1"] = 6
	lane_id_map["+,+,3,1"] = 7
	lane_id_map["+,-,3,1"] = 7
	lane_id_map["+,+,3,2"] = 8
	lane_id_map["+,-,3,2"] = 8
	lane_id_map["+,+,3,3"] = 9
	lane_id_map["+,-,3,3"] = 9
	lane_id_map["+,+,2,2"] = 10
	lane_id_map["-,+,2,2"] = 10
	lane_id_map["+,+,2,1"] = 11
	lane_id_map["-,+,2,1"] = 11
	lane_id_map["+,+,4,11"] = 12
	lane_id_map["-,+,4,11"] = 12
	lane_id_map["+,+,4,1"] = 13
	lane_id_map["-,+,4,1"] = 13
	lane_id_map["+,+,4,2"] = 14
	lane_id_map["-,+,4,2"] = 14
	lane_id_map["-,+,3,2"] = 15
	lane_id_map["-,-,3,2"] = 15
	lane_id_map["-,+,3,1"] = 16
	lane_id_map["-,-,3,1"] = 16
	lane_id_map["-,+,1,1"] = 17
	lane_id_map["-,-,1,1"] = 17
	lane_id_map["-,+,1,2"] = 18
	lane_id_map["-,-,1,2"] = 18
	lane_id_map["-,+,1,3"] = 19
	lane_id_map["-,-,1,3"] = 19
	lane_id_map["-,+,1,4"] = 20
	lane_id_map["-,-,1,4"] = 20
	lane_id_map["+,-,4,2"] = 21
	lane_id_map["-,-,4,2"] = 21
	lane_id_map["+,-,4,1"] = 22
	lane_id_map["-,-,4,1"] = 22
	#'''
	''' -> 11th street
	lane_id_map["+,-,2,2"] = 10
	lane_id_map["-,-,2,2"] = 10
	lane_id_map["+,-,2,1"] = 11
	lane_id_map["-,-,2,1"] = 11
	lane_id_map["+,-,4,11"] = 12
	lane_id_map["-,-,4,11"] = 12
	lane_id_map["+,-,4,1"] = 13
	lane_id_map["-,-,4,1"] = 13
	lane_id_map["+,-,4,2"] = 14
	lane_id_map["-,-,4,2"] = 14
	lane_id_map["+,+,1,1"] = 23
	lane_id_map["+,-,1,1"] = 23
	lane_id_map["+,+,3,1"] = 24
	lane_id_map["+,-,3,1"] = 24
	lane_id_map["+,+,2,2"] = 25
	lane_id_map["-,+,2,2"] = 25
	lane_id_map["+,+,2,1"] = 26
	lane_id_map["-,+,2,1"] = 26
	lane_id_map["+,+,4,11"] = 27
	lane_id_map["-,+,4,11"] = 27
	lane_id_map["+,+,4,1"] = 28
	lane_id_map["-,+,4,1"] = 28
	lane_id_map["+,+,4,2"] = 29
	lane_id_map["-,+,4,2"] = 29
	lane_id_map["-,+,3,1"] = 30
	lane_id_map["-,-,3,1"] = 30
	lane_id_map["-,+,1,1"] = 31
	lane_id_map["-,-,1,1"] = 31
	lane_id_map["-,+,1,2"] = 32
	lane_id_map["-,-,1,2"] = 32
	#'''
	#''' -> 12th street
	lane_id_map["+,+,1,1"] = 35
	lane_id_map["+,-,1,1"] = 35
	lane_id_map["+,+,3,1"] = 34
	lane_id_map["+,-,3,1"] = 34
	lane_id_map["+,+,2,2"] = 36
	lane_id_map["-,+,2,2"] = 36
	lane_id_map["+,+,2,1"] = 37
	lane_id_map["-,+,2,1"] = 37
	lane_id_map["+,+,4,11"] = 38
	lane_id_map["-,+,4,11"] = 38
	lane_id_map["+,+,4,1"] = 39
	lane_id_map["-,+,4,1"] = 39
	lane_id_map["+,+,4,2"] = 40
	lane_id_map["-,+,4,2"] = 40
	lane_id_map["-,+,1,2"] = 41
	lane_id_map["-,-,1,2"] = 41
	lane_id_map["-,+,1,1"] = 42
	lane_id_map["-,-,1,1"] = 42
	lane_id_map["-,+,3,1"] = 43
	lane_id_map["-,-,3,1"] = 43
	lane_id_map["-,+,3,2"] = 44
	lane_id_map["-,-,3,2"] = 44
	lane_id_map["+,-,2,2"] = 25
	lane_id_map["-,-,2,2"] = 25
	lane_id_map["+,-,2,1"] = 26
	lane_id_map["-,-,2,1"] = 26
	lane_id_map["+,-,2,11"] = 33
	lane_id_map["-,-,2,11"] = 33
	lane_id_map["+,-,4,1"] = 28
	lane_id_map["-,-,4,1"] = 28
	lane_id_map["+,-,4,2"] = 29
	lane_id_map["-,-,4,2"] = 29
	#'''
	lane_id_str = ""
	
	if float(spl[4]) > 0.0:
		lane_id_str += "+,"
	else:
		lane_id_str += "-,"
	
	if float(spl[5]) > 1240.0:# -- 12th street 725.73:# -- for 11th street #224.0:# -- 10th street   # 1240.0: -- 12th street
		lane_id_str += "+,"
	else:
		lane_id_str += "-,"
	
	lane_id_str += str(int(spl[18]))
	lane_id_str += ","
	lane_id_str += str(int(spl[13]))
	
	# generate the actual vehicle object
	
	ret = structs.Vehicle_Trajectory_Information()
	
	ret.vehicle_id = spl[0]
	ret.frame_id = int(spl[1])
	ret.global_time = int(spl[3])
	ret.x_offset_in_feet = float(spl[4])
	ret.y_offset_in_feet = float(spl[5]) #- 1000 # subtraction for 12th street only
	ret.latitude = float(spl[6])
	ret.longitude = float(spl[7])
	ret.speed_in_miles_per_hour = float(spl[11]) * 3600.0 / 5280 # Need to convert feet per second to miles per hour
	ret.acceleration_in_miles_per_hour_squared = float(spl[12]) * 3600 * 3600 / 5280 # Need to convert feet per second squared to miles per hour squared
	ret.vehicle_length_in_feet = float(spl[8])
	ret.vehicle_width_in_feet = float(spl[9])
	
	
	if x_bottom_left_feet_offset <= ret.x_offset_in_feet and y_bottom_left_feet_offset <= ret.y_offset_in_feet and x_top_right_feet_offset >= ret.x_offset_in_feet and y_top_right_feet_offset >= ret.y_offset_in_feet:
		if int(spl[13]) > 11:
			return None
		
		''' -> for 10th street
		if lane_id_str == "+,-,3,4":
			return None
		
		if lane_id_str == "+,+,2,11":
			return None
		
		if lane_id_str == "+,-,2,3":
			return None
		
		if lane_id_str == "-,+,2,11":
			return None
		
		if lane_id_str == "+,+,2,3":
			return None
		
		if lane_id_str == "-,+,3,11":
			return None
		
		if lane_id_str == "+,+,1,11":
			return None
		
		if lane_id_str == "+,+,3,11":
			return None
		
		#'''
		''' -> for 11th street
		if lane_id_str == "+,+,2,11":
			return None
		
		if lane_id_str == "-,-,2,11":
			return None
		
		if lane_id_str == "+,-,2,3":
			return None
		
		if lane_id_str == "+,-,2,11":
			return None
		
		if lane_id_str == "-,-,3,11":
			return None
		
		if lane_id_str == "-,-,3,2":
			return None
		
		if lane_id_str == "-,+,3,2":
			return None
		
		if lane_id_str == "-,+,2,11":
			return None
		
		if lane_id_str == "+,+,1,2":
			return None
		
		if lane_id_str == "+,+,3,2":
			return None
		
		if lane_id_str == "+,+,3,11":
			return None
		
		if lane_id_str == "-,+,3,11":
			return None
		#'''
		#''' -> for 12th street
		if lane_id_str == "+,+,2,11":
			return None # turn lane 13th street
		
		if lane_id_str == "-,-,4,11":
			return None # turn lane 11th street
		
		if lane_id_str == "+,-,4,11":
			return None # closest match: lane 38 (++411) -> also turning lane 13th street
		
		if lane_id_str == "+,+,1,11":
			return None # closest match: lane 35 (++11)
		
		if lane_id_str == "+,+,3,11":
			return None # closest match: lane 34 (++31)
		
		if lane_id_str == "+,+,3,2":
			return None # closest match: lane 44 (-+32)
		
		if lane_id_str == "+,-,1,2":
			return None # closest match: lane 37 (-+12)
		
		if lane_id_str == "-,+,2,11":
			return None # closest match: lane 33 (--211)
		
		if lane_id_str == "+,-,3,2":
			return None # closest match: lane 44 (--32)
		
		if lane_id_str == "+,-,3,11":
			return None # closest match: lane 34 (+-31)
		
		if lane_id_str == "-,-,3,11":
			return None # closest match: lane 43 (--31)
		
		if lane_id_str == "+,+,1,2":
			return None # closest match: lane 35 (++11)
		
		if lane_id_str == "+,+,3,3":
			return None
		
		if lane_id_str == "-,+,1,11":
			return None
		
		if lane_id_str == "+,+,2,3":
			return None
		#'''
		if int(spl[13]) != 0:
			lane_id = lane_id_map[lane_id_str]
			ret.lane_id = lane_id # -1 # int(spl[13])
		else:
			ret.lane_id = -1
		
		return ret
	
	return None

# parse a file into a list of into a list of signal timings
# Note: you should use parse_signal_file or parse_signal but not both
# this function will be called on every execution; return None if you prefer to use parse signal
def parse_signal_file(f, fname):
	f_attributes = fname.split("_")
	#if f_attributes[-1] == "1245-0100.txt":
	#	return None
	
	# Note: will need to improve lane id finder
	lane_id = int(f_attributes[1][0:2]) * 10
	lane_id_direction = ["EB", "SB", "WB", "NB"]
	'''
	12th street
	EB = 43, 44
	SB = 38, 39, 40
	WB = 35
	NB = 25, 26, 33
	'''
	'''
	10th street
	EB = 17, 18, 19, 20
	SB = 12, 13, 14
	WB = 7, 8, 9
	NB = 1, 2, 3
	'''
	'''
	11th street
	EB = 31, 32
	SB = 27, 28, 29
	WB = 24
	NB = 10, 11
	'''
	'''
	14th street
	EB = 65, 66
	SB = 60, 61, 62
	WB = 55, 56, 57
	NB = 48, 49, 50
	'''
	lane_id += lane_id_direction.index(f_attributes[2])
	
	lane_ids = []
	
	if f_attributes[2] == "EB":
		lane_ids = [43, 44]
	elif f_attributes[2] == "SB":
		lane_ids = [38, 39, 40]
	elif f_attributes[2] == "NB":
		lane_ids = [25, 26, 33]
	else:
		lane_ids = [35]
	
	import playback_data_model as structs
	signal_list = []
	contents = f.read()
	#valid_signal_states = ["Green-Left_Arrow", "Yellow-Left_Arrow", "Red-Left_Arrow", "Green-Ball", "Yellow-Ball", "Red-Ball"]
	valid_signal_states = ["Green-Ball", "Yellow-Ball", "Red-Ball"]
	
	entries = contents.split()
	entries = entries[3:] # remove the column headers
	
	# figure out which column the first entry belongs in
	lines = contents.split("\n")
	
	entries = ["0"] + entries + ["0"]
	
	
	for lane_id in lane_ids:
		curr_index = len(valid_signal_states) - len(lines[1].split()) - 1 # subtract 1 to get back to the one before the first entry (which must be the one to come before our current signal state)
		for i in range(len(entries) - 1):
			signal = structs.Single_Lane_Signal_State()
			signal.lane_id = lane_id
			signal.signal_start_step = int(entries[i])
			signal.signal_end_step = int(entries[i + 1]) - 1
			signal.current_signal_state = valid_signal_states[curr_index]
			signal_list.append(signal)
			curr_index += 1
			curr_index %= len(valid_signal_states)
	
	return signal_list
'''
160
EB = Red
NB = Green
SB = Red
WB = Red

1600
EB = Green
NB = Red
SB = RED
WB = Green
'''
# a function for parsing a single line of the signal file into a Single_Lane_Signal_State
# Note: you should use parse_signal_file or parse_signal but not both
# this function will be called only when parse_signal_file returns None
def parse_signal(line, fname):
	return None

def parse_map(f, fname):
	import playback_data_model as structs
	ret = structs.Map_Data()
	data = f.read()
	lines = filter(lambda x: x != "", data.split("\n"))
	
	l_nodes = map(parseLnodes, lines)
	unique_lane_ids = reduce(insert_unique, l_nodes, [])
	
	fil_lanes = map(lambda x: filter(lambda y: x == y.lane_id, l_nodes), unique_lane_ids)
	#print str(reduce(lambda b, c: b + "\n\n" + c, map(lambda a: reduce(lambda x, y: x + get_node_str(y) + "\n", a, ""), fil_lanes), ""))
	ln_lst = []#map(lambda x: structs.Lane_Data(), range(len(fil_lanes)))
	
	for l in fil_lanes:
		ret.lane_data.append(structs.Lane_Data())
	
	#for lane in ret.lane_data:
	for i in range(len(ret.lane_data)):
		lane = ret.lane_data[i]
		lane.center_points = []
		lane.widths_from_center = []
		l_lst = fil_lanes[i]
		for l in l_lst:
			cen = []
			cen.append(l.x)
			cen.append(l.y)
			lane.center_points.append(cen)
			lane.widths_from_center.append(l.width)
			lane.lane_id = l.lane_id
	
	#ret.lane_data = map(gen_lane, fil_lanes)#, ln_lst)
	#cen_points = map(get_centers, fil_lanes)
	#wids = map(get_wids, fil_lanes
	
	print "num nodes = " + str(len(l_nodes))
	print "num nodes after = " + str(reduce(lambda x, y: x + len(y.center_points), ret.lane_data, 0))
	
	return ret

def gen_lane(l_lst):
	import playback_data_model as structs
	ret = structs.Lane_Data()
	for l in l_lst:
		cen = []
		cen.append(l.x)
		cen.append(l.y)
		ret.center_points.append(cen)
		ret.widths_from_center.append(l.width)
		ret.lane_id = l.lane_id
	#ret = reduce(fold_into_lane, l_lst, ret)
	#print str(ret.center_points)
	#print str(ret.widths_from_center)
	return ret

def fold_into_lane(lane, node):
	lane.center_points.append([node.x, node.y])
	lane.widths_from_center.append(node.width)
	lane.lane_id = node.lane_id
	return lane

def parseLnodes(line):
	l = line.split(",")
	ret = internal_lane()
	
	ret.lane_id = int(l[2])
	ret.width = float(l[3])
	ret.x = float(l[0])
	ret.y = float(l[1])
	
	return ret

def insert_unique(lst, l_node):
	if l_node.lane_id not in lst:
		lst.append(l_node.lane_id)
	
	return lst

def get_node_str(node):
	ret = "lane_id = "
	ret += str(node.lane_id)
	ret += "\nx = "
	ret += str(node.x)
	ret += "\ny = "
	ret += str(node.y)
	ret += "\nwidth = "
	ret += str(node.width)
	ret += "\n"
	return ret

class internal_lane:
	lane_id = 0
	width = 0.0
	x = 0.0
	y = 0.0
	
	def __init__(self):
		pass
	
