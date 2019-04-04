


import peach_tree_parser as parser


def get_unique(lst, veh):
	if veh.vehicle_id not in lst:
		lst.append(veh.vehicle_id)
	
	return lst

f = open("/home/ablatt/NGSIMData/peachtree-main-data/vehicle-trajectory-data/0400pm-0415pm/trajectories-0400pm-0415pm.txt", 'r')
#f = open("/home/ablatt/NGSIMData/peachtree-main-data/vehicle-trajectory-data/1245pm-0100pm/trajectories-1245pm-0100pm.txt", 'r')

#vehs = map(lambda x: parser.parse_vehicle_trajectory(x, "trajectories-1245pm-0100pm.txt"), f)
vehs = map(lambda x: parser.parse_vehicle_trajectory(x, "trajectories-0400pm-0415pm.txt"), f)


f.close()
# for 12th street
all_tracked_lanes = [[38, 39, 40], [35], [33, 26, 25], [43, 44]]
#tracked_lanes = [33, 26, 25] # leg 3
#tracked_lanes = [35] # leg 2
#tracked_lanes = [38, 39, 40] # leg 1
#tracked_lanes = [43, 44] # leg 4
# for 10th street
#all_tracked_lanes = [[12, 13, 14], [7, 8, 9], [1, 2, 3], [17, 18, 19, 20]]
#tracked_lanes = [10, 11, 12, 13, 14] # leg 1
#tracked_lanes = [9, 8, 7, 6, 5, 4] # leg 2
#tracked_lanes = [22, 21, 1, 2, 3] # leg 3
#tracked_lanes = [15, 16, 17, 18, 19, 20] # leg 4
# for 11th street
#all_tracked_lanes = [[27, 28, 29], [24], [10, 11], [31, 32]]
#tracked_lanes = [25, 26, 27, 28, 29] # leg 1
#tracked_lanes = [23, 24] # leg 2
#tracked_lanes = [10, 11, 12, 13, 14] # leg 3
#tracked_lanes = [30, 31, 32] # leg 4

#print str(len(vehs))
#print vehs

vehs = filter(lambda x: x != None, vehs)

#print str(len(vehs))
for tracked_lanes in all_tracked_lanes:
	unique_veh_ids = reduce(get_unique, vehs, [])
	veh_lsts = map(lambda x: filter(lambda y: y.vehicle_id == x, vehs), unique_veh_ids)
	veh_lst_2 = map(lambda x: filter(lambda y:y.lane_id in tracked_lanes, x), veh_lsts)
	veh_max_lst = map(lambda veh: reduce(lambda mx, v: max(mx, v.speed_in_miles_per_hour), veh, 0), veh_lst_2)
	veh_max_lst = filter(lambda v: v > 0, veh_max_lst)
	veh_max_lst = sorted(veh_max_lst)
	print "num vehs = " + str(len(veh_max_lst))
	'''
	g = open("/home/ablatt/max_speeds.txt", 'w')
	map(lambda veh: g.write(str(veh) + "\n"), veh_max_lst)
	g.close()
	'''
	#print veh_max_lst
	if len(veh_max_lst) == 0:
		mean = 0.0
	else:
		mean = reduce(lambda x, y: x + y, veh_max_lst)/float(len(veh_max_lst))
	
	if len(veh_max_lst) == 0:
		eight_five = 0.0
	else:
		eight_five = veh_max_lst[int(len(veh_max_lst) * 0.85)]
	
	print tracked_lanes
	print "mean = " + str(mean)
	print "85th = " + str(eight_five)
	print "\n"
