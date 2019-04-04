The MicroscopicModelMOEs can be run by executing:
	make moes
to build the executable a.exe. Then run:
	./a.exe < moes.in

Below is an excerpt from the input file with comments on what the values are:

2 0.0 0.5 // (number of steps) (initial sim time) (step size)
1 // (number of lanes)
1 0.0 0.0 // 1 line for each lane with: (lane id) (x coord of base node) (y coord of base node)
3 // number of vehicles in first step
7 25.0 0.0 0.0 2.0 80.2 30.5 1 90.0 20.8 4 1.0 2.0 3.0 // 1 line for each vehicle with: 
// (vehicle id) (x) (y) (z) (speed) (length) (width) (lane id) (heading) (height) (vehicle type, as an int corresponding to the enum in EtexasNativeDataModel.h) (latitude) (longitude) (elevation)
11 30.0 0.0 0.0 2.1 80.2 30.5 1 90.0 20.8 4 1.0 2.0 3.0
13 900.0 0.0 0.0 2.1 80.2 30.5 1 90.0 20.8 4 1.0 2.0 3.0
1 // number of signals in first step
1 2 0 1 1.0 // 1 line for each signal with: (lane id) (color) (type) (state) (time to change), where color, type, and state are ints corresponding to the enums in EtexasNativeDataModel.h
3 // number of vehicles in second step
7 23.0 0.0 0.0 2.0 80.2 30.5 1 90.0 20.8 4 1.0 2.0 3.0
11 27.0 0.0 0.0 2.1 80.2 30.5 1 90.0 20.8 4 1.0 2.0 3.0
13 450.0 0.0 0.0 2.1 80.2 30.5 1 90.0 20.8 4 1.0 2.0 3.0