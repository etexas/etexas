/**
 * *****************************************************************************
 * Copyright (c) 2016 Harmonia Holdings Group LLC and others. All rights
 * reserved. This program and the accompanying materials are made available
 * under the terms of the Harmonia Partner License v1.0 which accompanies this
 * distribution, and is available at http://www.harmonia.com/legal/hpl-v10.html
 *
 * Contributors: Harmonia Holdings Group LLC - initial API and implementation
 ******************************************************************************
 */

/*
 * File:   test-messages.c
 * Author: ttevendale
 *
 * Created on May 12, 2016
 */

// C headers
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <MapData.h>
#include <SPAT.h>
#include <BasicSafetyMessage.h>

// libwave headers
#include "wave.h"
#include "intellifusion_data_model.h"
#include "message_parser.h"

/**
 * Main method
 *
 * @param argc Number of arguments.
 * @param argv Vector of arguments.
 * @return Status code.
 */
int main(int argc, char** argv) {
  printf("Invoking WAVE driver in main.c\n");
  if (invokeWAVEDevice(WAVEDEVICE_LOCAL, 0) < 0) {
    printf("Open Failed. Quitting\n");
    exit(-1);
  }
asn_dec_rval_t rval;
MapData_t *map_dat = 0;
SPAT_t *spat = 0;
//@37.2047777, -80.4142872,21z
  LaneManager lm;
  
  lm.lane_count = 0;
  lm.latitude = 37.2047777;
  lm.longitude = -80.4142872;

  int ret_int = 0, i, j;
  WSMIndication ind;
  WSMRequest req;

//	GPSData gps_data;

//	get_gps_status(&gps_data, "app");

//	double secondsSince1970 = gps_data.actual_time;

  while (1) {
//		printf("SimTime for tx: %f\n", GetSimTime());

//		PrintMessages();
//		PrintSignalData();
//		PrintLaneData();
//		PrintDetectorData();



//		ret_int = txWSMPacket(0, &req);

    ret_int = rxWSMPacket(0, &ind);
    printf("Message length: %d\n", ind.data.length);
//		printf("Message bytes: ", ind.data.contents);
    for(i = 0;i < ind.data.length;i++) {
      printf("%02.2hhX", ind.data.contents[i]);
    }
    printf("\n\n");
    fflush(stdout);

/*		Vehicle* veh = ParseBsm(ind.data.contents, ind.data.length, &lm);
		printf("after parse bsm\n");
		fflush(stdout);
		if(veh) {
			printf("id = %d\n", veh->vehicle_id);
			printf("speed = %lg\n", veh->speed);
			printf("length = %lg\n", veh->length);
			printf("width = %lg\n", veh->width);
			printf("latitude = %lg\n", veh->latitude);
			printf("longitude = %lg\n", veh->longitude);
			printf("x = %lg\n", veh->x);
			printf("y = %lg\n", veh->y);
			printf("z = %lg\n", veh->z);
			printf("lane_id = %lg\n", veh->lane_id);
			printf("heading = %lg\n", veh->heading);
			printf("height = %lg\n", veh->height);
			printf("vehicle_type = %lg\n", veh->vehicle_type);
			printf("elevation = %lg\n", veh->elevation);
			free(veh);
		} else {
			printf("failed to free\n");
		}
*/
/*		SignalIndicationList* sil = ParseSpat(ind.data.contents, ind.data.length);

		while(sil != NULL){
			printf("lane_id = %d\n", sil->signal->lane_id);
			printf("intersection_id = %d\n", sil->signal->intersection_id);
			printf("color = %d\n", sil->signal->color);
			printf("type = %d\n", sil->signal->type);
			printf("state = %d\n", sil->signal->state);
			printf("time_to_change = %f\n\n", sil->signal->time_to_change);
			sil = sil->next;
		}
*/
		LaneManagerList* lml = ParseMapData(ind.data.contents, ind.data.length);

		while(lml != NULL){
			LaneManager* lm = lml->lane_manager;
			printf("\nlatitude = %f\n", lm->latitude);
			printf("longitude = %f\n", lm->longitude);
			printf("elevation = %f\n", lm->elevation);
			printf("lane_count = %d\n\n", lm->lane_count);
			fflush(stdout);
			for(i = 0; i < lm->lane_count; i++){
				Lane l = lm->lanes[i];
				printf("  lane type = %d\n", l.type);
				printf("  id = %d\n", l.id);
				printf("  node_count = %d\n\n", l.node_count);
				fflush(stdout);
					for(j = 0; j < l.node_count; j++){
						LaneNode ln = l.nodes[j];
						printf("    x = %f\n", ln.x);
						printf("    y = %f\n", ln.y);
						printf("    z = %f\n", ln.z);
						printf("    width = %f\n\n", ln.width);
						fflush(stdout);
					}
				printf("movement_count = %d\n", l.movement_count);
				for(j = 0; j < l.movement_count; j++){
					printf("movementType = %d\n", l.movements[j]);
				}
				printf("\n");
			}
			printf("intersection_id = %d\n\n", lm->intersection_id);
			lml = lml->next;
		}
//rval = asn_DEF_MapData.ber_decoder(0, &asn_DEF_MapData, (void**) &map_dat,
//  ind.data.contents, ind.data.length, 0);
//rval = asn_DEF_SPAT.ber_decoder(0, &asn_DEF_SPAT, (void **) &spat, ind.data.contents,
//	ind.data.length, 0);
//printf("rval: %d\n", rval);
//fflush(stdout);

    sleep(1);
  }
  return 0;
}
