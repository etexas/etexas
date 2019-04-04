

def get_dats_from_file(s):
	return map(lambda x: x.split(","), s.split("\n"))

def convert_to_csv(l):
	new_lines = map(lambda x: reduce(lambda y, z: y + "," + z, x), l)
	return reduce(lambda x, y: x + "\n" + y, new_lines)

def shift_map_point(point, shift_amount):
	ret = []
	
	ret.append("-1.0")
	ret.append("-1.0")
	ret.append("-1.0")
	ret.append("-1.0")
	
	ret[1] = point[1]
	ret[2] = point[2]
	ret[3] = point[3]
	
	ret[0] = str(float(point[0]) - shift_amount)
	
	return ret

down_shift_amount = 1000

in_map = open("/home/ablatt/NGSIMData/peachtree-main-data/cad-diagram/peach-12th-shifted-2.csv", 'r')
out_map = open("/home/ablatt/NGSIMData/peachtree-main-data/cad-diagram/peach-12th-shifted-3.csv", 'w')

m = in_map.read()
dats = get_dats_from_file(m)
out_dats = map(lambda x: shift_map_point(x, down_shift_amount), dats)
out_map.write(convert_to_csv(out_dats))

in_map.close()
out_map.close()
