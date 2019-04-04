
import peach_tree_parser as parser

def get_max_veh(veh1, veh2):
	if veh1.y_offset_in_feet < veh2.y_offset_in_feet:
		return veh2
	else:
		return veh1

def parse_veh(v):
	pass

def parse_vehs(f):
	pass

f = open("/home/ablatt/NGSIMData/peachtree-main-data/vehicle-trajectory-data/0400pm-0415pm/trajectories-0400pm-0415pm.txt", 'r')

vehs = map(lambda x: parser.parse_vehicle_trajectory(x, "trajectories-0400pm-0415pm.txt"), f)

vehs = filter(lambda v: v != None, vehs)

f.close()

lanes_to_examine = [1, 2, 3]

fil1 = map(lambda x: filter(lambda y: y.lane_id == x, vehs), lanes_to_examine)

fil1 = filter(lambda x: len(x) > 0, fil1)

maxes = map(lambda x: reduce(get_max_veh, x), fil1)

for m in maxes:
	print "lane id = " + str(m.lane_id) + "   y = " + str(m.y_offset_in_feet)

#print maxes

#class Veh
