
#include "EtexasNativeDataModel.h"
#include "ParseMessages.h"
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>

// * ablatt - uncomment for testing
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
	return 1;
}

/*int main() {
	char *file_bytes;
	
	printf("Starting Parsing Fun\nParsing BSM\n");
	
	file_bytes = readFileBytes("/home/ablatt/bsmoutput.dat");
	int len = getFileLength("/home/ablatt/bsmoutput.dat");
	//printf(file_bytes);
	//printf("\n");
	
	Vehicle* veh = parseBSM(file_bytes, len, 0.0, 0.0);
	print_vehicle(veh);
	
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
	//printf(file_bytes);
	//printf("\n");
	
	Signal_Indication_List* sigs = parseSPAT(file_bytes, len);
	print_signal_indication_list(sigs);
	free_signal_indication_list(sigs);
	
	free(file_bytes);

	file_bytes = readFileBytes("/home/ablatt/map_data_files.ber");
	len = getFileLength("/home/ablatt/map_data_files.ber");
	//printf(file_bytes);
	//printf("\n");
	
	Lane_Manager_Linked_List *lan_man = parseMapData(file_bytes, len);
	print_lane_manager(lan_man->data);
	free_lane_manager_linked_list(lan_man);
	free(file_bytes);
	
	printf("Finishing Parsing SPAT\nFinishing Parsing Fun\n");
	return 0;
}*/

