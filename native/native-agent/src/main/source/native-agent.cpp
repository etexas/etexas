/**
 * *****************************************************************************
 * Copyright (c) 2014 Harmonia Holdings Group LLC and others. All rights
 * reserved. This program and the accompanying materials are made available
 * under the terms of the Harmonia Partner License v1.0 which accompanies this
 * distribution, and is available at http://www.harmonia.com/legal/hpl-v10.html
 *
 * Contributors: Harmonia Holdings Group LLC - initial API and implementation
 ******************************************************************************
 */

/* 
 * File:   native-agent.cpp
 * Author: bbadillo
 *
 * Created on September 18, 2014, 1:51 PM
 */


/*
 MACRO to use for debug output. 
 */
#if ENABLE_DEBUG
    #define DEBUG_LOG printf
#else
    #define DEBUG_LOG(...)
#endif

// C headers
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>

// C++ headers
#include <iostream>
#include <sstream>
#include <map>
#include <list>

// Protocol Buffers headers
#include "NativeAppProtobuf.pb.h"

// NanoMsg headers
#include <nanomsg/nn.h>
#include <nanomsg/reqrep.h>
#include <nanomsg/pubsub.h>
#include <nanomsg/pipeline.h>

using namespace std;
using namespace etexas;
using namespace google::protobuf;

/*
 * Forward Declarations
 */
char *CheckAndResizeSendBuffer(char *, int &, int);
void PrintBundleWrapperData(BundleWrapper);
void AddAppOutputBundleToGlobalMap(map<long, BundleWrapper*>*, BundleWrapper*);
void CloseAgent(int arg = 0);
void Error(const char *);

/*
 * Global variable to use for terminating the main loop. 
 */
static bool volatile isRunning = true;

/**
 * Main method
 * 
 * @param argc Number of arguments. 
 * @param argv Vector of arguments. 
 * @return Status code. 
 */
int main(int argc, char** argv) {
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	// Hook to the interrupt signal and gracefully shutdown agent. 
	signal(SIGINT, CloseAgent);
	signal(SIGTERM, CloseAgent);

	int sock_fd, port_num;
	int pub_fd;
	int pull_fd;
	char exec_id_str[20]; // A 8-byte long value can be no longer than 20 digits in decimal (including a sign)
	char dev_id_str[20]; // A 8-byte long value can be no longer than 20 digits in decimal (including a sign)
	char *buffer = NULL;
	int array_size = 0;
	char *array = new char[array_size];

	map <int64, list <pid_t> > exec_pid_map; // Map of processes started keyed by execution id. 
	map <int64, BundleWrapper*> output_wrapper_map; // Map of output bundles keyed by execution id. 
	const char ETEXAS_LIB_EXECID[] = "ETEXAS_LIB_EXECID";
	const char ETEXAS_LIB_APPID[] = "ETEXAS_LIB_APPID";
	const char ETEXAS_LIB_DEVID[] = "ETEXAS_LIB_DEVID";

	BundleWrapper bundle_wrapper;
	BundleWrapper app_output_wrapper;
	
	if (argc < 3) {
		Error("ERROR, Usage: command host port\n");
	}
	port_num = atoi(argv[2]);

	// Create REPLY socket
	sock_fd = nn_socket(AF_SP, NN_REP);
	if (sock_fd < 0) {
		Error("ERROR on REPLY socket creation");
	}
	// Create PUBLISH socket
	pub_fd = nn_socket(AF_SP, NN_PUB);
	if (pub_fd < 0) {
		Error("ERROR on PUBLISH socket creation");
	}
	// Create PULL socket
	pull_fd = nn_socket(AF_SP, NN_PULL);
	if (pull_fd < 0) {
		Error("ERROR on PULL socket creation");
	}

	// Setup a server for publishing data to native apps. 
	if (nn_bind(pub_fd, "ipc:///tmp/pubsub.ipc") < 0) {
		Error("ERROR on binding pubsub");
	}
	// Setup a server for reading data from native apps. 
	if (nn_bind(pull_fd, "ipc:///tmp/pipeline.ipc") < 0) {
		Error("ERROR on binding pipeline");
	}
	
	stringstream url_stream;
	url_stream << "tcp://" << argv[1] << ":" << port_num;
	if (nn_bind(sock_fd, url_stream.str().c_str()) < 0) {
		Error("ERROR on binding");
	}

	// Enter server loop. 
	while (isRunning) {

		// Block until a message is received, then parse into a protocol buffer. 
		int num_bytes = nn_recv(sock_fd, &buffer, NN_MSG, 0);
		DEBUG_LOG("num_bytes %d/n", num_bytes);

		// Now parse the rest of the message into a protocol buffer and free the original buffer. 
		bool parsed = bundle_wrapper.ParseFromArray(buffer, num_bytes);
		nn_freemsg(buffer);
		if (!parsed) {
			Error("Bundle Wrapper Message failed to parse.");
		}
		long exec_id = bundle_wrapper.execid();
		sprintf(exec_id_str, "%ld", exec_id);

		// Print the protocol buffer for test purposes. 
		DEBUG_LOG("BundleWrapper: %s\n", bundle_wrapper.DebugString().c_str());
		
		// Get outputs to send back
		while((num_bytes = nn_recv (pull_fd, &buffer, NN_MSG, NN_DONTWAIT)) >= 0) {
			DEBUG_LOG("num_bytes %d/n", num_bytes);
			if(num_bytes >= 0) {
				parsed = app_output_wrapper.ParseFromArray(buffer, num_bytes);
				if (!parsed) {
					Error("App Layer Output Message failed to parse.");
				}
				AddAppOutputBundleToGlobalMap(&output_wrapper_map, &app_output_wrapper);
				nn_freemsg (buffer);
			}
		}
		
		// Get the outputs to send back for the execution that was just sent.
		BundleWrapper* output_wrapper = output_wrapper_map[exec_id];
		if(output_wrapper != NULL) {
			output_wrapper_map.erase(exec_id);
			int output_wrapper_size = output_wrapper->ByteSize();
			array = CheckAndResizeSendBuffer(array, array_size, output_wrapper_size);
			bool success = output_wrapper->SerializeToArray(array, output_wrapper_size);
			delete output_wrapper;
			// Send a reply with outputs.
			nn_send(sock_fd, array, output_wrapper_size, 0);			
		} else {
			// Send an empty reply.
			nn_send(sock_fd, "\0", 1, 0);
		}

		// Switch on the type of message received. 
		if (bundle_wrapper.has_initbundle()) {
			// Initialize execution
			InitBundle initbundle = bundle_wrapper.initbundle();

			// Iterate through the list of apps to start for the execution and track the new process pids. 
			list<pid_t> pid_list;
			for (int i = 0; i < initbundle.appinitconfig_size(); i++) {
				AppInitConfig appinitconfig = initbundle.appinitconfig(i);
				string app_id_string = appinitconfig.appid();
				long dev_id = appinitconfig.devid();
				sprintf(dev_id_str, "%ld", dev_id);
				string command_line = appinitconfig.commandline();

				// turn the command line into an array of command tokens
				stringstream stream(command_line);
				vector<char *> tokens;
				string token;

				while (getline(stream, token, ' ')) {
				  tokens.push_back(strdup(token.c_str()));
				}

				tokens.push_back(NULL);
				char **command = &tokens[0];

				// Fork a new process from the Native Agent. 
				pid_t pid = fork();
				if (pid == 0) {
					// This is the new process. 
					// Redirect standard out and error to a file. 
					int fd = open(app_id_string.c_str(), O_RDWR | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR);
					dup2(fd, 1);
					dup2(fd, 2);
					close(fd);

					// Set an environment variable to hold the exec id, app id, and dev id for the new process. 
					setenv(ETEXAS_LIB_EXECID, exec_id_str, 1);
					setenv(ETEXAS_LIB_APPID, app_id_string.c_str(), 1);
					setenv(ETEXAS_LIB_DEVID, dev_id_str, 1);

					// Execute the native app
					printf("Starting Native App %s for exec_id=%s\n", command[0], exec_id_str);
					int exec_ret = execvp(command[0], command);
					stringstream ss;
					ss << "Error for exec " << exec_ret << " while running this executable: " << command_line << endl;
					ss << "Make sure " << command_line << " is on the path." << endl;					
					Error(ss.str().c_str()); // If errors happen on execute of app. This thread must die!

				} else {
					// This is still the Native Agent, so track the forked process. 
					pid_list.push_back(pid);
					DEBUG_LOG("Started process %d/n", pid);
				}
			}

			// Map the started processes to the exec id so they can be terminated later. 
			exec_pid_map[exec_id] = pid_list;

		} else if (bundle_wrapper.has_inputbundle()) {
			// Distribute timestep data to native apps. 
			InputBundle input_bundle = bundle_wrapper.inputbundle();
			
			// Encode the exec id into the first bytes of the message as a
			// null-terminated string. 
			size_t msg_index = strlen(exec_id_str) + 1; // Add 1 byte to the size for the null-terminated string value. 
			int bundle_size = bundle_wrapper.ByteSize();
			array = CheckAndResizeSendBuffer(array, array_size, bundle_size + msg_index);
			
			// Copy the exec id into the message for sending. 
			strcpy(array, exec_id_str);
			
			// Write the protocol buffer data into the buffers.
			bool success = bundle_wrapper.SerializeToArray(&array[msg_index], bundle_size);
			int ret_int = nn_send(pub_fd, array, bundle_size + msg_index, 0);
			if (ret_int < 0) {
				Error("ERROR on sending input data");
			}
		} else if (bundle_wrapper.has_shutdownbundle()) {
			// Shutdown execution
			ShutdownBundle shutdown_bundle = bundle_wrapper.shutdownbundle();

			// Terminate all the processes tracked for a particular exec id. 
			list<pid_t> pid_list = exec_pid_map[exec_id];
			for (list<pid_t>::iterator iter = pid_list.begin(); iter != pid_list.end(); iter++) {
				pid_t pid = *iter;
				if (kill(pid, SIGTERM) == 0) {
					DEBUG_LOG("Terminated process %d\n", pid);
				}
			}
			exec_pid_map.erase(exec_id);
		}
	}

	// Shutdown all open connections and cleanup memory usage. 
	nn_shutdown(sock_fd, 0);
	nn_shutdown(pub_fd, 0);
	nn_shutdown(pull_fd, 0);
	delete [] array;

	google::protobuf::ShutdownProtobufLibrary();

	return 0;
}

/**
 * Make sure that the byte buffer for sending messages is large enough. 
 * If not allocate a buffer of the size needed. 
 * 
 * @param buffer Pointer to the buffer to check and resize. 
 * @param buffer_size Current size of the buffer. Will be set to the new size if buffer is resized.
 * @param size_needed The size that is needed for the buffer. 
 * @return The checked (and possibly resized) buffer. 
 */
char *CheckAndResizeSendBuffer(char *buffer, int &buffer_size, int size_needed) {
	if (size_needed > buffer_size) {
		delete [] buffer;
		buffer_size = size_needed;
		return new char[size_needed];
	} else {
		return buffer;
	}
}

/**
 * Add an output received from an app to a running map for organizational purposes. 
 * 
 * @param bundle_map The map to use for organization. 
 * @param app_output_wrapper The app output to save in the map. 
 */
void AddAppOutputBundleToGlobalMap(map<int64, BundleWrapper*>* bundle_map, BundleWrapper* app_output_wrapper) {
	// Get the execution id of the app output. 
	int64 exec_id = app_output_wrapper->execid();
	DEBUG_LOG("AppOutputBundle: %s\n", app_output_wrapper->DebugString().c_str());
	
	// Check if the map already contains an entry for the execution id. 
	if(bundle_map->count(exec_id) == 0) {
		// If the map does not contain the execution id, then add a new bundle. 
		BundleWrapper* output_wrapper = new BundleWrapper();
		*output_wrapper = *app_output_wrapper;
		(*bundle_map)[exec_id] = output_wrapper;
	} else {
		// If the map does contain the execution id, then keep the bundle and add
		// the app outputs to it. 
		BundleWrapper* output_wrapper = (*bundle_map)[exec_id];
		for(int i = 0; i < app_output_wrapper->outputbundle_size(); i++) {
			OutputBundle* output_bundle = output_wrapper->add_outputbundle();
			*output_bundle = app_output_wrapper->outputbundle(i);
		}
	}
}

/**
 * Set the running flag to false so that the main loop exits. 
 * 
 * @param arg A dummy argument. 
 */
void CloseAgent(int arg) {
	isRunning = false;
}

/**
 * Stop the program and print the error message. 
 * 
 * @param msg A message to print to standard error.
 */
void Error(const char *msg) {
	perror(msg);
	exit(1);
}


