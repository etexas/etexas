# author: ablatt

# A file containing utility functions primarily used in playback_parser.py

# a function for printing the file types
def print_file_types(file_types):
	print "file types:"
	
	for f in file_types:
		print f

# a function for getting the locations of the files from the command line arguments
# returns a dictionary of the locations of the files keyed on the file type
def get_file_locations(args, file_types):
	ret = {}
	
	for a in args:
		s = a.split("=")
		
		if len(s) == 2:
			if len(s[0]) > 2:
				s[0] = s[0][2:]
				
				if s[0] not in file_types:
					print "--" + s[0] + " is not a valid file type"
					print_file_types(file_types)
					clean_and_exit()
				else:
					ret[s[0]] = s[1]
		else:
			print "argument " + a + " is malformed"
			print "proper format is --MY_FILE_TYPE=/path/to/file"
			print_file_types(file_types)
			clean_and_exit()
	
	return ret

# a function for checking if the all the files passed into the playback parser via the command line actually exist and are of the proper type (folder)
# this function cleans and exits if any of the files passed in do not exist or are of the wrong type
def check_exists(file_locations):
	import os.path # for checking the existence of files
	
	for key in file_locations:
		loc = file_locations[key]
		if os.path.exists(loc):
			if os.path.isdir(loc):
				print loc + " has been found"
			else:
				print loc + " is not a folder (but was found)"
				#clean_and_exit()
		else:
			print loc + " could not be found"
			clean_and_exit()

# a function for parsing a vehicle trajectory folder
def parse_vehicle_trajectory(fpath):
	import peach_tree_parser as parser
	import playback_write_out as write_out
	import os
	
	to_parse = os.listdir(fpath)
	i = 0
	
	for path in to_parse:
		f = open(fpath + os.sep + path)
		
		trajectory_list = parser.parse_vehicle_trajectory_file(f, path)
		
		if trajectory_list == None:
			f.close() # refresh the file handle
			f = open(fpath + os.sep + path)
			
			for line in f:
				trajectory = parser.parse_vehicle_trajectory(line, path)
				write_out.write_vehicle_trajectory(trajectory)
				i += 1
				
				if (i % 1000) == 0:
					print "processed " + str(i) + " vehicles"
				
			print "processed " + str(i) + " vehicles"
		else:
			for trajectory in trajectory_list:
				write_out.write_vehicle_trajectory(trajectory)
		
		f.close()
	

# a function for parsing a signal folder
# returns the list of signal states
def parse_signal(fpath):
	import peach_tree_parser as parser
	import playback_write_out as write_out
	import os
	
	to_parse = os.listdir(fpath)
	ret = []
	
	for path in to_parse:
		f = open(fpath + os.sep + path)
		
		signal_list = parser.parse_signal_file(f, path)
		
		if signal_list == None:
			f.close()
			f = open(fpath + os.sep + path)
			signal_list = []
			for line in f:
				signal = parser.parse_signal(line, path)
				if not signal == None:
					signal_list.append(signal)
		else:
			ret += signal_list
		
		f.close()
		
	return ret

# a function for parsing a map folder
def parse_map(fpath):
	import peach_tree_parser as parser
	import playback_write_out as write_out
	f = open(fpath, 'r')
	m = parser.parse_map(f, fpath)
	f.close()
	write_out.write_map_data(m)

# a function for parsing a detector folder
# returns the list of detector locations
def parse_detector(fpath):
	pass
	

# cleans and exits the program
def clean_and_exit():
	import playback_write_out as write_out
	write_out.clean_destroy()
	exit()

# cleans the generated files so they are ready to be zipped
def clean_files_for_zip():
	import playback_write_out as write_out
	write_out.clean_step_data()
	write_out.clean_static_data()

# zips the output folder
def zip_files():
	import zipfile
	import os
	
	out_zip = zipfile.ZipFile("playback_output.zip", 'w')
	
	perform_zip(populate_dir("playback_temp_output_folder"), out_zip)
	
	out_zip.close()

# a function which builds a tree which holds all the paths within curr path
def populate_dir(curr_path):
	import os
	import os.path
	
	if os.path.isdir(curr_path):
		return [curr_path] + map(lambda x, y: populate_dir(y + os.sep + x), os.listdir(curr_path), [curr_path] * len(os.listdir(curr_path)))
	else:
		return curr_path

# recursively zips the tree built by populate_dir
# dir_structure - the results from populate_dir
# out - the zipfile object to write to
def perform_zip(dir_structure, out):
	for d in dir_structure:
		if isinstance(d, list):
			perform_zip(d, out)
		else:
			out.write(d)
























