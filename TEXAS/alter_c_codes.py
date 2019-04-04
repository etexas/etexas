
# Author: Tony Blatt

'''

 *** *                                                            * ***
 *** *  Copyright (c) 2012 Harmonia Holdings Group LLC            * ***
 *** *                                                            * ***
 *** * Permission is hereby granted to use, modify, copy, and     * ***
 *** * distribute this software and its documentation for any     * ***
 *** * purpose only without profit, provided that the above       * ***
 *** * Copyright Notice appears in all copies and that both the   * ***
 *** * Copyright Notice and this Permission Notice appears in     * ***
 *** * every copy of supporting documentation.  No title to nor   * ***
 *** * ownership of the software is transferred hereby.  The name * ***
 *** * of Harmonia Holdings Group LLC shall not be used in        * ***
 *** * advertising or publicity related to the distribution of    * ***
 *** * the software without specific, written, prior permission.  * ***
 *** * This software is provided as-delivered without expressed   * ***
 *** * or implied warranty.  Harmonia Holdings Group LLC          * ***
 *** * makes no representation about the suitability of this      * ***
 *** * software for any purpose and accepts no responsibility for * ***
 *** * its use.                                                   * ***
 *** *                                                            * ***
 *** ************************************************************** ***
 *** *                                                            * ***
 *** * This program is free software; you can redistribute it     * ***
 *** * and/or modify it under the terms of the GNU General Public * ***
 *** * License as published by the Free Software Foundation;      * ***
 *** * either version 2 of the License, or (at your option) any   * ***
 *** * later version.                                             * ***
 *** *                                                            * ***
 *** * This program is distributed in the hope that it will be    * ***
 *** * useful, but WITHOUT ANY WARRANTY; without even the implied * ***
 *** * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    * ***
 *** * PURPOSE.  See the GNU General Public License for more      * ***
 *** * details.                                                   * ***
 *** *                                                            * ***
 *** * You should have received a copy of the GNU General Public  * ***
 *** * License along with this program; if not, write to the Free * ***
 *** * Software Foundation, Inc., 51 Franklin Street, Fifth       * ***
 *** * Floor, Boston, MA 02110-1301, USA.                         * ***
 *** *                                                            * ***
 *** * For more information: http://www.gnu.org/licenses/gpl.html * ***
 *** *                                                            * ***

'''

# ASSUMPTION: We assume that, if the output directory exists, nothing has been changed in the output directory
# INDICATES: We will exit if the output directory already exists
# INDICATES: You must delete the output directory if you want a fresh C code change
# NOTE: You can use "mvn clean" to delete the output directory
# ASSUMPTION: We assume that all C codes which create a comment are to be replaced with 2 spaces
# ASSUMPTION: We assume that all C codes which are noted starting in column 73 are to replace 2 spaces at the front of that line
# INDICATES: It is not possible to have C codes on the same line as a 4 digit label
# INDICATES: C codes must start in column 73 if they are not being used as a comment
# ASSUMPTION: We assume that ' is an invalid character in a C code (due to our inability to use it on the command line)

# imports
import os # importing to easily make directories
import os.path # importing to get file names easily
import sys # for fetching command line arguements

#--------------------------------------------------
# functions used in command_line_arg_functions
# functions parse command line args into a usable form (see "for arg in args:" for details)

# parses --CCODES= into individual C codes and returns the resulting list of C codes
# arg_split - a list of strings in the format ["--CCODES", C_CODE_ELEMENTS]
# return - a list of the form ["c_codes", LIST_OF_C_CODES] - this will form a key-value pair to append onto the value in user_defined_values for that key
def parse_c_codes(arg_split):
	codes = []
	
	for letter in arg_split[1]:
		codes.append("C" + letter)
	
	return ["c_codes", codes]

# parses --SRC= into a source path
# arg_split - a list of strings in the format ["--SRC", SOURCE_PATH]
# return - a list of the form ["source_folder", SOURCE_PATH] - this will form a key-value pair to append onto the value in user_defined_values for that key
def parse_source(arg_split):
	return ["source_folder", arg_split[1]]

# parses --DEST= into a source path
# arg_split - a list of strings in the format ["--DEST", DESTINATION_PATH]
# return - a list of the form ["destination_folder", DESTINATION_PATH] - this will form a key-value pair to append onto the value in user_defined_values for that key
def parse_destination(arg_split):
	return ["destination_folder", arg_split[1]]

# displays help and quits the program
# arg_split - irrelavent
def display_help_and_exit(arg_split):
	print "usage: alter_c_codes.py --SRC=[path to source folder] --DEST=[path to destination folder] [options]"
	print "Valid options are the following:"
	print "--CCODES=[your C codes here]"
	print "--linux							automatically includes the CCODES necessary for compilation on Linux (which are .]{} )"
	print "--windows						automatically includes the CCODES necessary for compilation on Windows (which are ... )"
	print "--mac							automatically includes the CCODES necessary for compilation on OSX (which are ... )"
	print " "
	print "General usage notes:"
	print "The path to the source folder must be a valid path to a directory on the local machine"
	print "The destination folder cannot be a directory on the local machine"
	print "The path leading to the destination folder must be a valid path to a directory on the local machine"
	print "If multiple parameters of the same type appear on the command line, the last occurance of the parameter is used"
	print "(--CCODES=, --linux, --windows and --mac are all the same type of parameter)"
	exit()

#--------------------------------------------------
# global variables
command_line_arg_functions = {"--CCODES": parse_c_codes, "--SRC": parse_source, "--DEST": parse_destination, "--help": display_help_and_exit}
special_c_code_combinations = {"--linux": ".]{}", "--windows": "insert here", "--mac": "insert here"}
# special CCode candidates:
# ),.]{ - from rioux_run_removec_on_c_simpro4_src_to_for_file_windows_compaq.bat
# Note: I have not been able to find any other CCode combinations according to the scripts in chimera/products/eTEXAS/Architecture%20and%20Design%20Documents/External%20Artifacts/TEXAS%20info/Source

# According to the email chain, the following are the CCodes for their respective platforms
# ,.]{# - windows
# ,.]% - mac
# ,.]{} - linux -- this combination is different from the combination which Tony got to compile on his machine ( .]{} )

user_defined_values = {"source_folder": "", "destination_folder": "", "c_codes": []}

c_code_loc = 78
c_code_line_end = 72

max_size_to_read_from_file = 1000000

#--------------------------------------------------
# function to copy a fortran file
# fortran_file_path - the location of the file to copy
# output_file_path - the location to copy the file into
def process_fortran_file(fortran_file_path, output_file_path):
	f = open(fortran_file_path, 'r')
	g = open(output_file_path, 'w')
	
	for line in f:
		if (len(line) > 78) and (line[72:78] == "CCODE="):
			code = line[c_code_loc:]
			code = code.strip()
			line = code + line[2:c_code_line_end] + "\n"
		
		code = line[0:2]
		
		if code in user_defined_values["c_codes"]:
			line = line.strip()
			line = "  " + line[2:]
			
			while len(line) < c_code_line_end:
				line += " "
			
			line += "CCODE=" + code + "\n"
		
		g.write(line)
	
	g.close()
	f.close()

# function to copy a file
# inpath - the location of the file to copy
# outpath - the location to copy the file into
def copy_file(inpath, outpath):
	size = os.path.getsize(inpath)
	
	f = open(inpath, 'rb')
	g = open(outpath, 'wb')
	
	while size >= 0:
		bytes = f.read(max_size_to_read_from_file)
		g.write(bytes)
		size -= max_size_to_read_from_file
	
	g.close()
	f.close()

# Process a single file
# fpath - path to get to the file to process
# out_path - path to put the duplicate of that file into
# splitter - the character used to split directories on a path for the current os (e.g. / for linux and \ for windows)
# no return value
# ASSUMPTION: We assume that out_path alreay has its path splitter 
def process_file(fpath, out_path, splitter):
	split = os.path.split(fpath)
	fname = split[1]
	out_file_path = out_path + fname
	
	if os.path.isdir(fpath):
		# we have a folder
		curr_dir = os.listdir(fpath)
		os.mkdir(out_file_path)
		out_file_path += splitter
		
		for c in curr_dir:
			process_file(fpath + splitter + c, out_file_path, splitter)
		
	elif fname[-3:] == "for":
		# we have a fortran file
		process_fortran_file(fpath, out_file_path)
	else:
		# we have a non-fortan file
		copy_file(fpath, out_file_path)

#--------------------------------------------------
# fetch and parse the arguments
args = sys.argv[1:]

for arg in args:
	sa = arg.split("=")
	if arg in special_c_code_combinations:
		comb = special_c_code_combinations[arg]
		
		for letter in comb:
			code = "C" + letter
			user_defined_values["c_codes"].append(code)
	elif sa[0] in command_line_arg_functions:
		tmp = command_line_arg_functions[sa[0]](sa)
		user_defined_values[tmp[0]] = tmp[1]
	else:
		# special case for a bad argument
		# display error and exit
		print "The following arg is unrecognized: "
		print arg + "\n"
		print "Exiting without action"
		print "Proper usage is the following:"
		display_help_and_exit([])

# print out relevant information
print "source path = " + user_defined_values["source_folder"]
print "destination path = " + user_defined_values["destination_folder"]
print "C codes = " + str(user_defined_values["c_codes"])

split = os.path.split(user_defined_values["source_folder"])
separator = user_defined_values["source_folder"][len(split[0])]

# check that ll the folders are in the right place (or not as the case may be)
if not os.path.exists(user_defined_values["source_folder"]):
	print "Input must contain a proper path to a source folder (SRC)"
	print "Proper usage is the following:"
	display_help_and_exit([])

if not os.path.isdir(user_defined_values["source_folder"]):
	print "The source folder (SRC) must be a folder on the local machine"
	print "Proper usage is the following:"
	display_help_and_exit([])

if os.path.exists(user_defined_values["destination_folder"]):
	print "The destination folder (DEST) cannot exist on the machine"
	print "Proper usage is the following:"
	display_help_and_exit([])

if user_defined_values["destination_folder"][-1] == separator:
	spl = os.path.split(user_defined_values["destination_folder"][0:-1])
	
	if not os.path.exists(spl[0]):
		print "The path leading up to the destination folder must be a valid path"
		print "Proper usage is the following:"
		display_help_and_exit([])
	
	if not os.path.isdir(spl[0]):
		print "The path leading up to the destination folder must have a folder as its last link on the path"
		print "Proper usage is the following:"
		display_help_and_exit([])
else:
	spl = os.path.split(user_defined_values["destination_folder"])
	
	if not os.path.exists(spl[0]):
		print "The path leading up to the destination folder must be a valid path"
		print "Proper usage is the following:"
		display_help_and_exit([])
	
	if not os.path.isdir(spl[0]):
		print "The path leading up to the destination folder must have a folder as its last link on the path"
		print "Proper usage is the following:"
		display_help_and_exit([])

if not (user_defined_values["destination_folder"][-1] == separator):
	user_defined_values["destination_folder"] += separator

if not (user_defined_values["source_folder"][-1] == separator):
	user_defined_values["source_folder"] += separator

os.mkdir(user_defined_values["destination_folder"])

# go through the contents of the source folder one at a time
sub_source_folder = os.listdir(user_defined_values["source_folder"])

for sub in sub_source_folder:
	sub = user_defined_values["source_folder"] + sub
	process_file(sub, user_defined_values["destination_folder"], separator)
















