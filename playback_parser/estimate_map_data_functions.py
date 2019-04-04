# author: ablatt

# this file contains the functions actually used to estimate the map data
# a separate file was created for estimating map data because the the code for estimating map data was deemed too large to be mixed with other code

# this function estimates the map data from the vehicle trajectory data
def estimate_map_data_function():
	import playback_write_out as write_out
	import playback_data_model as structs
	
	first_step, last_step = write_out.get_min_and_max_steps()
	
	# get all known lane numbers
	print "getting a list of lane ids"
	known_lane_ids = write_out.get_lane_ids(first_step, last_step)
	
	# get the set of centers and widths
	map_data = structs.Map_Data()
	centers = []
	widths = []
	for lane_id in known_lane_ids:
		# get the first box
		print "getting the points in lane " + str(lane_id)
		points_in_lane = get_points_in_lane(lane_id, first_step, last_step)
		
		starting_point = get_starting_point(points_in_lane)
		x, y, width, height = define_box(starting_point.x_offset_in_feet, starting_point.y_offset_in_feet)
		# get the first center and initial shifts
		points_in_rec = get_points_in_rectangle(x, y, width, height, points_in_lane)
		center = get_mean(points_in_rec)
		shift = get_shift(points_in_rec)
		centers.append(center)
		widths.append(get_width(center, points_in_rec))
		# shift on the lower end until the shift no longer has more points
		points_lower_base = quick_clone(points_in_rec)
		x_lower = starting_point.x_offset_in_feet - 12.0
		y_lower = starting_point.y_offset_in_feet - 12.0
		
		lower_shift = shift
		
		while 42:
			print "shifting " + str(lower_shift[0]) + " by " + str(lower_shift[1])
			if lower_shift == [0, 0, 0, 0]:
				break
			
			n_x_lower, n_y_lower, new_box = perform_shift(lower_shift[0], lower_shift[1], x_lower, y_lower, points_in_lane)
			
			if shift_is_not_new(new_box,[n_x_lower, n_y_lower, 24.0, 24.0], [x_lower, y_lower, 24.0, 24.0]):
				break
			
			if len(new_box) < 20:
				break
			
			center = get_mean(new_box)
			lower_shift = get_shift(new_box)
			centers.append(center)
			widths.append(get_width(center, new_box))
			points_lower_base = new_box
			
			x_lower = n_x_lower
			y_lower = n_y_lower
		
		del x_lower
		del y_lower
		del points_lower_base
		del lower_shift
		
		# shift on the upper end until the shift no longer has more points
		points_upper_base = quick_clone(points_in_rec)
		x_upper = starting_point.x_offset_in_feet - 12.0
		y_upper = starting_point.y_offset_in_feet - 12.0
		upper_shift = shift
		
		while 42:
			print "shifting " + str(upper_shift[2]) + " by " + str(upper_shift[3])
			if upper_shift == [0, 0, 0, 0]:
				break
			
			n_x_upper, n_y_upper, new_box = perform_shift(upper_shift[2], upper_shift[3], x_upper, y_upper, points_in_lane)
			
			if shift_is_not_new(new_box, [n_x_upper, n_y_upper, 24.0, 24.0], [x_upper, y_upper, 24.0, 24.0]):
				break
			
			if len(new_box) < 20:
				break
			
			center = get_mean(new_box)
			upper_shift = get_shift(new_box)
			centers.append(center)
			widths.append(get_width(center, new_box))
			points_lower_base = new_box
			
			x_upper = n_x_upper
			y_upper = n_y_upper
		
		del x_upper
		del y_upper
		del points_upper_base
		del upper_shift
		
		# add lane centers to the map_data
		lane = structs.Lane_Data()
		lane.lane_id = lane_id
		centers = sort_centers(centers, points_in_lane)
		lane.center_points = centers
		lane.widths_from_center = widths
		map_data.lane_data.append(lane)
		centers = []
		widths = []
	
	write_out.write_map_data(map_data)

# this function retrieves all the points in lane lane_id between first_step and last_step
# returns a list of Vehicle_Trajectory_Information
def get_points_in_lane(lane_id, first_step, last_step):
	import playback_write_out as write_out
	import playback_data_model as structs
	ret = []
	
	for i in range(first_step, last_step + 1):
		step = write_out.repopulate_step(i)
		
		for obj in step:
			if isinstance(obj, structs.Single_Lane_Signal_State) or isinstance(obj, structs.Detector_State):
				pass
			elif isinstance(obj, structs.Vehicle_Trajectory_Information):
				if obj.lane_id == lane_id:
					ret.append(obj)
	
	return ret

# a function for getting the starting point out of a lane to start the finding centers process
def get_starting_point(points_in_lane):
	return points_in_lane[0]

# (x, y) of the bottom left of the box
# get all the points in the box defined by x, y, width, height
# points_in_lane can be sub-set of the points actually in the lane (or even points from several lanes if you prefer)
def get_points_in_rectangle(x, y, width, height, points_in_lane):
	bl_x = x
	bl_y = y
	tr_x = x + width
	tr_y = y + height
	
	ret = []
	
	for point in points_in_lane:
		x = point.x_offset_in_feet
		y = point.y_offset_in_feet
		
		if x >= bl_x and y >= bl_y and x <= tr_x and y <= tr_y:
			ret.append(point)
	
	return ret

# get the mean of the point list (finds the mean x value and mean y value of the list of Vehicle_Trajectory_Information)
def get_mean(point_list):
	x_sum = 0.0
	y_sum = 0.0
	
	for point in point_list:
		x_sum += point.x_offset_in_feet
		y_sum += point.y_offset_in_feet
	
	return [x_sum/len(point_list), y_sum/len(point_list)]

# gets the slope which the center of the box should be shifted by
def get_shift(point_list):
	vehicle_id_list = list_vehicles(point_list)
	shift_list = []
	for vehicle in vehicle_id_list:
		single_shift = get_shift_for_vehicle(vehicle, point_list)
		if not single_shift == "None":
			shift_list.append(single_shift)
	
	return ave_shift(shift_list)

# gets a list of all vehicle ids in point list
def list_vehicles(point_list):
	ret = []
	for point in point_list:
		if point.vehicle_id not in ret:
			ret.append(point.vehicle_id)
	
	return ret

# gets the forward and backward trajectory of the vehicle weighted toward the trajectories near the end of the vehicle's trajectory
def get_shift_for_vehicle(vehicle_id, point_list):
	# build two shifts
	delta_x_lower = None
	delta_y_lower = None
	delta_x_upper = None
	delta_y_upper = None
	curr_lower = None
	curr_higher = None
	seen_two = False
	for i in range(len(point_list)):
		if point_list[i].vehicle_id == vehicle_id:
			if curr_lower == None:
				curr_lower = point_list[i]
			else:
				dxl = point_list[i].x_offset_in_feet - curr_lower.x_offset_in_feet
				dyl = point_list[i].y_offset_in_feet - curr_lower.y_offset_in_feet
				
				if delta_x_lower == None:
					delta_x_lower = dxl
					delta_y_lower = dyl
				else:
					delta_x_lower = (delta_x_lower + dxl) / 2
					delta_y_lower = (delta_y_lower + dyl) / 2
					seen_two = True
				
				curr_lower = point_list[i]
		
		if point_list[-i - 1].vehicle_id == vehicle_id:
			if curr_higher == None:
				curr_higher = point_list[-i - 1]
			else:
				dxu = point_list[-i - 1].x_offset_in_feet - curr_higher.x_offset_in_feet
				dyu = point_list[-i - 1].y_offset_in_feet - curr_higher.y_offset_in_feet
				
				if delta_x_upper == None:
					delta_x_upper = dxu
					delta_y_upper = dyu
				else:
					delta_x_upper = (delta_x_upper + dxu) / 2
					delta_y_upper = (delta_y_upper + dyu) / 2
				
				curr_higher = point_list[-i - 1]
	
	
	if not seen_two:
		return "None"
	
	return [delta_x_lower, delta_y_lower, delta_x_upper, delta_y_upper]

# averages a list of shifts together into a single shift
def ave_shift(shift_list):
	if len(shift_list) == 0:
		return [0, 0, 0, 0]
	
	ave_shift = []
	
	for shift in shift_list:
		if ave_shift == []:
			for i in shift:
				ave_shift.append(i)
		else:
			for i in range(4):
				ave_shift[i] += shift[i]
	
	for i in range(4):
		ave_shift[i] /= len(shift_list)
	
	return ave_shift

# creates a definition of a box (x, y, width, height) from a center point (x, y)
def define_box(x, y):
	return [x - 12.0, y - 12.0, 24.0, 24.0]

# clones a list
def quick_clone(point_list):
	ret = []
	
	for point in point_list:
		ret.append(point)
	
	return ret

# performs the shift of the box along the slope delta_x/delta_y and returns the points
# in the new bottom left corner of the box along with the points in the new box
def perform_shift(delta_x, delta_y, center_x, center_y, points_in_lane):
	# get the new x, y of the center of the new box
	center_x, center_y = shift_by_dist(10.0, delta_y, delta_x, center_x, center_y)
	
	# get the populated points for that box
	points_in_rec = get_points_in_rectangle(center_x, center_y, 24.0, 24.0, points_in_lane)
	
	return [center_x, center_y, points_in_rec]

# checks to see if the new point list contains any new points
def shift_is_not_new(new_point_list, point_area_old, point_area_new):
	import math
	
	overlap_rect = []
	overlap_rect.append(max(point_area_new[0], point_area_old[0]))
	overlap_rect.append(max(point_area_new[1], point_area_old[1]))
	overlap_rect.append(point_area_new[2] - math.fabs(point_area_new[0] - point_area_old[0]))
	overlap_rect.append(point_area_new[3] - math.fabs(point_area_new[1] - point_area_old[1]))
	
	sub_points = get_points_in_rectangle(overlap_rect[0], overlap_rect[1], overlap_rect[2], overlap_rect[3], new_point_list)
	
	if len(sub_points) < len(new_point_list):
		return False
	
	return True

# sorts the centers by y offset
def sort_centers(centers, points_in_lane):
	for i in range(len(centers)):
		for j in range(i + 1, len(centers)):
			if centers[j][1] < centers[i][1]:
				tmp = centers[j]
				centers[j] = centers[i]
				centers[i] = tmp
	
	return centers

# estimates the width of the points around the center within the box
def get_width(center, box):
	vehicle_ids = []
	
	for vehicle in box:
		if vehicle.vehicle_id not in vehicle_ids:
			vehicle_ids.append(vehicle.vehicle_id)
	
	deltas = {}
	
	for i in vehicle_ids:
		curr = None
		delta_x = None
		delta_y = None
		count = 0
		
		for j in range(len(box)):
			if box[j].vehicle_id == i:
				if curr == None:
					curr = box[j]
				else:
					dxl = box[j].x_offset_in_feet - curr.x_offset_in_feet
					dyl = box[j].y_offset_in_feet - curr.y_offset_in_feet
					
					if delta_x == None:
						delta_x = dxl
						delta_y = dyl
						count += 1
					else:
						delta_x = delta_x + dxl
						delta_y = delta_y + dyl
						count += 1
					
					curr = box[j]
		if not delta_x == None:
			deltas[i] = [delta_x / count, delta_y / count]
	
	ave_x = 0.0
	ave_y = 0.0
	
	for key in deltas:
		ave_x += deltas[key][0]
		ave_y += deltas[key][1]
	
	if ave_x == 0.0 or ave_y == 0:
		return handle_vertical_box(ave_x, ave_y, center[0], center[1], box)
	
	ave_x /= len(deltas)
	ave_y /= len(deltas)
	
	slope = ave_y / ave_x
	
	normal_x = -ave_y
	normal_y = ave_x
	
	normal = normal_y / normal_x
	
	point_upper_long_x, point_upper_long_y = shift_by_dist(2.0, -ave_y, -ave_x, center[0], center[1])
	point_lower_long_x, point_lower_long_y = shift_by_dist(2.0, ave_y, ave_x, center[0], center[1])
	point_upper_short_x, point_upper_short_y = shift_by_dist(25.0, -normal_y, -normal_x, center[0], center[1])
	point_lower_short_x, point_lower_short_y = shift_by_dist(25.0, normal_y, normal_x, center[0], center[1])
	
	if point_upper_long_y < point_lower_long_y:
		tmp = point_upper_long_x
		point_upper_long_x = point_lower_long_x
		point_lower_long_x = tmp
		
		tmp = point_upper_long_y
		point_upper_long_y = point_lower_long_y
		point_lower_long_y = tmp
	
	if point_upper_short_y < point_lower_short_y:
		tmp = point_lower_short_y
		point_lower_short_y = point_upper_short_y
		point_upper_short_y = tmp
		
		tmp = point_upper_short_x
		point_upper_short_x = point_lower_short_x
		point_lower_short_x = tmp
	
	upper_long_b = point_upper_long_y - (normal * point_upper_long_x)
	lower_long_b = point_lower_long_y - (normal * point_lower_long_x)
	upper_short_b = point_upper_short_y - (slope * point_upper_short_x)
	lower_short_b = point_lower_short_y - (slope * point_lower_short_x)
	
	to_check = []
	
	for point in box:
		xy = get_coords(point)
		
		if is_below(xy, slope, upper_short_b) and is_below(xy, normal, upper_long_b) and is_above(xy, slope, lower_short_b) and is_above(xy, normal, lower_long_b):
			to_check.append(xy)
	
	return get_width_from_box(point_upper_short_x, point_lower_short_y, to_check)

# gets the distance between two points
def get_dist(x1, y1, x2, y2):
	return (((x1 - x2) ** 2) + ((y1 - y2) ** 2)) ** 0.5

# determines if point is below the line defined by m and b
# Note: y = mx + b
def is_below(point, m, b):
	line_y = (m * point[0]) + b
	return line_y > point[1]

# determines if point is above the point defined by m and b
# Note: y = mx + b
def is_above(point, m, b):
	return not is_below(point, m, b)

# shifts the point (start_x, start_y) by dist_to_move along the rise/run slope
def shift_by_dist(dist_to_move, rise, run, start_x, start_y):
	const = ((dist_to_move ** 2)/((run ** 2) + (rise ** 2))) ** 0.5
	run *= const
	rise *= const
	start_x += run
	start_y += rise
	
	return [start_x, start_y]

# this funciton handles the special case where the slope (ave_x/ave_y) is either completely vertical (ave_y = 0) or completely horizontal (ave_x = 0)
def handle_vertical_box(ave_x, ave_y, center_x, center_y, box):
	if ave_x == 0.0:
		# rise is entirely veritcal
		# width measured along the horizontal
		return get_width_from_box(center_x, center_y - 25.0, map(get_coords, get_points_in_rectangle(center_x - 25.0, center_y - 2.0, 50.0, 4.0, box)))
	else:
		# run is entirely horizontal
		# width is measured along the vertical
		return get_width_from_box(center_x - 25.0, center_y, map(get_coords, get_points_in_rectangle(center_x - 2.0, center_y - 25.0, 4.0, 50.0, box)))

# a helper function for extracting x, y coordinates from vehicle trajectories
def get_coords(point):
	return [point.x_offset_in_feet, point.y_offset_in_feet]

# this function gets the closest and furthest points in box from the base point then returns the distance between the closest and furthest point as the width of the box
def get_width_from_box(base_point_x, base_point_y, box):
	min_xy = None
	max_xy = None
	min_dist = 0.0
	max_dist = 0.0
	
	for xy in box:
		if min_xy == None:
			min_xy = xy
			max_xy = xy
			min_dist = get_dist(base_point_x, base_point_y, xy[0], xy[1])
			max_dist = min_dist
		else:
			dist_xy = get_dist(base_point_x, base_point_y, xy[0], xy[1])
			if min_dist > dist_xy:
				min_xy = xy
				min_dist = dist_xy
			elif max_dist < dist_xy:
				max_xy = xy
				max_dist = dist_xy
	
	return get_dist(min_xy[0], min_xy[1], max_xy[0], max_xy[1])
