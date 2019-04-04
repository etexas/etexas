
# author ablatt

import gen_functions as gen_funcs
import playback_write_out as write_out
import estimation_functions_generator as est
import utility_functions_playback_parser as utils

write_out.init_output()

map_data = gen_funcs.gen_map()
vehicle_data = gen_funcs.gen_vehicle_data()
detector_list = gen_funcs.gen_detector_list()
map(lambda x:write_out.write_vehicle_trajectory(x), vehicle_data)
detector_list = est.estimate_detector_data(detector_list)
signal_list = gen_funcs.gen_signal_list()

write_out.write_signal_list(signal_list)
write_out.write_extra_data(1000) # 1 second steps
write_out.write_detector_list(detector_list)
write_out.write_map_data(map_data)

utils.clean_files_for_zip()
