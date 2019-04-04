
def gen_lane(i):
	import playback_data_model as structs
	
	ret = structs.Lane_Data()
	
	ret.lane_id = i
	ret.lane_width_in_feet = 12
	ret.center_points = map(lambda x: [i, ((x + 1) * 20)], range(20))
	ret.widths_from_center = map(lambda x: 12, range(20))
	
	return ret

def gen_veh(i):
	import playback_data_model as structs
	
	ret = structs.Vehicle_Trajectory_Information()
	
	ret.vehicle_id = str(i % 10)
	ret.frame_id = int(i % 99)
	ret.global_time = i - int(i / 10)
	ret.vehicle_length_in_feet = 20
	ret.vehicle_width_in_feet = 1
	
	return ret

def gen_det(i):
	import playback_data_model as structs
	
	ret = structs.Detector_Internal()
	
	ret.detector_number = i
	ret.bottom_left_x = i - 3.0
	ret.bottom_left_y = i - 3.0
	ret.top_right_x = i + 3.0
	ret.top_right_y = i + 3.0
	
	return ret

def gen_signal(lane_id, sig_state, start_step, end_step):
	import playback_data_model as structs
	
	ret = structs.Single_Lane_Signal_State()
	
	ret.lane_id = lane_id
	ret.current_signal_state = sig_state
	ret.signal_start_step = start_step
	ret.signal_end_step = end_step
	
	return ret

def wrap_gen_signal(i):
	state = "no"
	j = i % 3
	lane_id = 0
	
	if i > 2:
		lane_id = 18
	
	if j == 0:
		state = "green-ball"
	elif j == 1:
		state = "yellow-ball"
	else:
		# j == 2
		state = "red-ball"
	
	return gen_signal(lane_id, state, (j * 2) + j, (j * 2) + j + 2)

def gen_map():
	import playback_data_model as structs
	
	ret = structs.Map_Data()
	ret.lane_data = map(gen_lane, [0, 18])
	
	return ret

def gen_vehicle_data():
	return map(lambda x: gen_veh(x), range(200))

def gen_detector_list():
	return map(gen_det, [0, 18])

def gen_signal_list():
	return map(wrap_gen_signal, range(6))
