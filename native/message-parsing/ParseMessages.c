
#include <sys/types.h>
#include <SPAT.h>
#include <BasicSafetyMessage.h>
#include <stdio.h>
#include <stdlib.h>

/*
 * Compile Command: gcc -I. -o aa_test
 */

typedef enum VEHICLE_TYPE {
	car = 4,
	bus = 6,
	tractor_trailer = 11
} veh_type;

typedef enum SIGNAL_COLOR {
	green,
	yellow,
	red,
	none
} sig_color;

typedef enum SIGNAL_TYPE {
	ball,
	left_arrow,
	right_arrow,
	straight_arrow,
	uturn_arrow,
	stop_sign,
	yield_sign,
	uncontrolled,
	unknown
} sig_type;

typedef enum SIGNAL_STATE {
	steady = 1,
	flashing,
	soft
} sig_state;

typedef struct Vehicle {
	int vehicle_id;
	double x; // in cm
	double y; // in cm
	double z; // in cm
	double speed; // in meters per second
	double length; // in cm
	double width; // in cm
	int lane_id;
	double heading;
	double height;
	veh_type vehicle_type;
	double latitude;
	double longitude;
	double elevation;
} Vehicle_t;

typedef struct Signal_Indication {
	int lane_id;
	sig_color color;
	sig_type type;
	sig_state state;
	double time_to_change;
} Signal_Indication_t;

int build_int(uint8_t* blob, int starting_index) {
	int ret = blob[starting_index] & 255;
	ret <<= 8;
	ret |= ((int)blob[starting_index + 1]) & 255;
	ret <<= 8;
	ret |= ((int)blob[starting_index + 2]) & 255;
	ret <<= 8;
	ret |= ((int)blob[starting_index + 3]) & 255;
	
	
	//ret = *((int*)(blob + starting_index));
	
	
	
	return ret;
}

Vehicle_t* blob_to_vehicle(uint8_t* blob) {
	int len_wid;
	Vehicle_t* ret = (Vehicle_t*)malloc(sizeof(struct Vehicle));
	
	
	ret->vehicle_id = build_int(blob, 1);
	ret->latitude = (build_int(blob, 7)/10000000.0);
	ret->longitude = build_int(blob, 11)/10000000.0;
	
	// lat and long conversion needs to happen here.
	
	int speed = blob[21];
	speed <<= 8;
	speed |= blob[22];
	ret->speed = (double)(speed & 0b1111111111111);
	ret->speed *= 0.02;
	
	int heading = blob[23];
	heading <<= 8;
	heading |= blob[24];
	
	len_wid = (int)blob[31];
	len_wid <<= 8;
	len_wid |= (int)blob[32];
	len_wid <<= 8;
	len_wid |= (int)blob[33];
	
	ret->length = (double)(len_wid & 0b11111111111111);
	ret->width = (double)(len_wid >> 14);
	
	// unparsed constants
	ret->vehicle_type = car;
	
	return ret;
}

/*
 * Note: you must manually free the input and output yourself.
 */
Vehicle_t* parseBSM(char* bytes, int size) {
	BasicSafetyMessage_t *bsm = 0;
	asn_dec_rval_t rval;
	Vehicle_t *ret;
	
	rval = asn_DEF_BasicSafetyMessage.ber_decoder(0, &asn_DEF_BasicSafetyMessage, (void **)&bsm, bytes, size, 0);
	if(rval.code == RC_OK) {
		ret = blob_to_vehicle(bsm->blob1.buf);
		printf("success\n"); // need to remove eventually
	} else {
		printf("failure\n"); // need to remove eventually
		ret = NULL;
	}
	
	asn_DEF_BasicSafetyMessage.free_struct(&asn_DEF_BasicSafetyMessage, bsm, 0);
	
	return ret;
}

/*
 * Note: you must manually free the input and output yourself.
 */
int parseSPAT(char* bytes, int size) {
	SPAT_t *spat = 0;
	asn_dec_rval_t rval;
	
	rval = asn_DEF_SPAT.ber_decoder(0, &asn_DEF_SPAT, (void **)&spat, bytes, size, 0);
	if(rval.code == RC_OK) {
		printf("success\n"); // need to remove eventually
	} else {
		printf("failure\n"); // need to remove eventually
	}
	
	// will need to do paring here...
	
	asn_DEF_SPAT.free_struct(&asn_DEF_SPAT, spat, 0);
	
	return 1;
}

/*
 * Note: you must free the return yourself.
 */
char* readFileBytes(const char *name) {
	FILE *fl = fopen(name, "r");  
	fseek(fl, 0, SEEK_END);  
	long len = ftell(fl);  
	char *ret = malloc(len);  
	fseek(fl, 0, SEEK_SET);  
	fread(ret, 1, len, fl);  
	fclose(fl);  
	return ret;  
}

int getFileLength(const char *name) {
	FILE *fl = fopen(name, "r");  
	fseek(fl, 0, SEEK_END);  
	long len = ftell(fl);
	return (int)len;
}

int main() {
	char *file_bytes;
	
	printf("Starting Parsing Fun\nParsing BSM\n");
	
	file_bytes = readFileBytes("/home/ablatt/bsmoutput.dat");
	int len = getFileLength("/home/ablatt/bsmoutput.dat");
	printf(file_bytes);
	printf("\n");
	
	Vehicle_t* veh = parseBSM(file_bytes, len);
	
	if(veh) {
		printf("id = %d\n", veh->vehicle_id);
		printf("speed = %lg\n", veh->speed);
		printf("length = %lg\n", veh->length);
		printf("width = %lg\n", veh->width);
		printf("latitude = %lg\n", veh->latitude);
		printf("longitude = %lg\n", veh->longitude);
		free(veh);
	} else {
		printf("failed to free\n");
	}
	
	free(file_bytes);
	
	printf("Finishing Parsing BSM\nParsing SPAT...\n");
	
	file_bytes = readFileBytes("/home/ablatt/spatoutput.ber");
	len = getFileLength("/home/ablatt/spatoutput.ber");
	printf(file_bytes);
	printf("\n");
	
	parseSPAT(file_bytes, len);
	
	free(file_bytes);
	
	printf("Finishing Parsing SPAT\nFinishing Parsing Fun\n");
	return 0;
}
