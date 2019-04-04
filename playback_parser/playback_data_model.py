# author: ablatt

# this is a file which contains all and only the data to be used in this script

#-----------------------------------------------------------
# this is the data structures to be used by this program
#-----------------------------------------------------------
# this class contains all the map data
class Map_Data:
	reference_x = 0.0
	reference_y = 0.0
	lane_data = [] # should contain only Lane_Data instances
	
	def __init__(self):
		pass

# this class contains all the data on a specific lane in the intersection
class Lane_Data:
	lane_id = -1
	is_egress = False
	lane_width_in_feet = -1.0
	lane_has_right_turn = False
	lane_has_right_turn_on_red = False
	lane_has_left_turn = False
	lane_has_left_turn_on_red = False
	lane_has_u_turn = False
	center_points = []
	widths_from_center = []
	connector_lanes = [] # list of lane_ids
	
	def __init__(self):
		pass

# this class contains the possible signal states
class Signal_Data:
	single_lane_data = [] # a list of Single_Lane_Signal_State
	
	def __init__(self):
		pass

# this class contains the possible states of a single signal
class Single_Lane_Signal_State:
	ingress_lane_id = -1
	is_a_stop_sign = False
	cars_can_go_left = []
	cars_can_go_right = []
	cars_can_go_straight = []
	cars_can_make_a_u_turn = []
	there_is_a_left_turn_arrow = False
	there_is_a_right_turn_arrow = False
	there_is_a_u_turn_arrow = False
	
	def __init__(self):
		pass

# this class contains all the information on a vehicle in a specific frame
class Vehicle_Trajectory_Information:
	vehicle_id = "your id here"
	frame_id = -1
	global_time = -1
	x_offset_in_feet = 0.0
	y_offset_in_feet = 0.0
	latitude = -1000.0
	longitude = -1000.0
	speed_in_miles_per_hour = 0.0
	acceleration_in_miles_per_hour_squared = 0.0
	vehicle_length_in_feet = -1.0
	vehicle_width_in_feet = -1.0
	lane_id = -1
	
	def __init__(self):
		pass

# this class is meant to hold the state of a single signal
class Single_Lane_Signal_State:
	lane_id = -1
	current_signal_state = "Undecided"
	signal_start_step = 0 # 0 for the first observed frame
	signal_end_step = -1 # -1 for the last observed frame
	
	def __init__(self):
		pass

# this class contains all the data on a single detector event
class Detector_State:
	detector_number = -1
	frame_id = -1
	vehicle_velocity_in_miles_per_hour = -1.0
	vehicle_length_in_feet = -1.0
	vehicle_width_in_feet = -1.0
	
	def __init__(self):
		pass

# a class for describing a detector to the detector simulator
class Detector_Internal:
	detector_number = -1
	bottom_left_x = -1.0
	bottom_left_y = -1.0
	top_right_x = -1.0
	top_right_y = -1.0
