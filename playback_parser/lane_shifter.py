

def get_dats_from_file(s):
	return map(lambda x: x.split(","), s.split("\n"))

def convert_to_csv(l):
	new_lines = map(lambda x: reduce(lambda y, z: y + "," + z, x), l)
	return reduce(lambda x, y: x + "\n" + y, new_lines)

def get_amount_to_shift_by(node, center_ids, dats):
	middle = filter(lambda x: x[0] == node[0], dats)
	n = filter(lambda x: int(x[1]) in center_ids, middle)
	
	if len(n) == 0:
		return 0.0
	
	return float(n[0][3]) / 2.0

def shift_node(node, shift_sign, center_ids, dats):
	ret = []
	
	ret.append("-1.0")
	ret.append("-1.0")
	ret.append("-1.0")
	ret.append("-1.0")
	
	ret[0] = node[0]
	ret[1] = node[1]
	ret[3] = node[3]
	
	amount = get_amount_to_shift_by(node, center_ids, dats)
	amount *= shift_sign
	ret[2] = str(float(node[2]) + amount)
	
	return ret
'''
make_center = [38, 45]
shift_negative = [39, 40, 45]
shift_positive = [37, 36, 38]
'''
make_center = [33, 27]
shift_negative = [33, 29, 28]
shift_positive = [27, 26, 25]

in_csv = open("/home/ablatt/NGSIMData/peachtree-main-data/cad-diagram/peach-12th-shifted-1.csv", 'r')
out_csv = open("/home/ablatt/NGSIMData/peachtree-main-data/cad-diagram/peach-12th-shifted-2.csv", 'w')

dats = get_dats_from_file(in_csv.read())
dats = dats[0:-1]
to_shift_positive = filter(lambda x: int(x[1]) in shift_positive, dats)
to_shift_negative = filter(lambda x: int(x[1]) in shift_negative, dats)

shifted_positive = map(lambda x: shift_node(x, 1.0, make_center, dats), to_shift_positive)
shifted_negative = map(lambda x: shift_node(x, -1.0, make_center, dats), to_shift_negative)
not_shifted = filter(lambda x: (int(x[1]) not in shift_positive) and (int(x[1]) not in shift_negative), dats)
out_txt = convert_to_csv(shifted_positive + shifted_negative + not_shifted)
out_csv.write(out_txt)

in_csv.close()
out_csv.close()


'''
shift_by_amount = 0
in_csv = open("", 'r')
out_csv = open("", 'w')

csv = in_csv.read()
lines = csv.split("\n")

vals = map(lambda x: x.split(","), lines)

for v in vals:
	v[2] = str(float(v[2]) - 7.0)

new_lines = map(lambda x: reduce(lambda y, z: y + "," + z, x), vals)
new_csv = reduce(lambda x, y: x + "n" + y, new_lines)

out_csv.write(new_csv)

in_csv.close()
out_csv.close()
'''
