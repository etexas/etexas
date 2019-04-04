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
 * File:   main.cpp
 * Author: bbadillo
 *
 * Created on November 7, 2014, 11:48 AM
 */

// C headers
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
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

  int ret_int = 0, i, j;
  WSMIndication ind;
//	WSMRequest req;

//	GPSData gps_data;

  LaneManagerList* lml = NULL;

  while (1) {
//		get_gps_status(&gps_data, "app");

//		double secondsSince1970 = gps_data.actual_time;
//		printf("start of app loop\nSimTime for tx: %d\n", secondsSince1970);
//		fflush(stdout);
//		PrintMessages();
//		PrintSignalData();
//		PrintLaneData();
//		PrintDetectorData();

//		ret_int = txWSMPacket(0, &req);
    printf("just before rxWSMPacket\n");
    fflush (stdout);
    ret_int = rxWSMPacket(0, &ind);

    printf("just after rxWSMPacket\n");
    fflush(stdout);
    printf("Message length: %d\n", ind.data.length);
//		printf("Message bytes: ", ind.data.contents);
    for (i = 0; i < ind.data.length; i++) {
      printf("%hhX", ind.data.contents[i]);
    }
    printf("\n\n");

    fflush(stdout);
    //char useBytes[] = {0x30, 0x2B, 0x80, 0x01, 0x02, 0x81, 0x26, 0x01, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    //			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07, 0xD1, 0x81, 0x00,
    //			0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
    //char useBytes[] = {0x30, 0x2B, 0x80, 0x01, 0x02, 0x81, 0x26, 0x01, 0x00, 0x00, 0x00, 0x01, 0xFF, 0xFF, 0xF2, 0xE3, 0x11, 0x00, 0x0D, 0xB5, 0x85,
    //			0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x42, 0xEE, 0x00, 0x50, 0x00, 0x00, 0x0A, 0x07, 0xD1, 0x81, 0x00, 0x00, 0x00,
    //			0x00, 0x00, 0x18, 0x03};
    if (lml == NULL) {
      lml = ParseMapData(ind.data.contents, ind.data.length);
      printf("after ParseMapData\n");
      fflush(stdout);
    }
    /*		while(lml != NULL){
     LaneManager* lm = lml->lane_manager;
     printf("lane_count = %d\n\n", lm->lane_count);
     fflush(stdout);
     for(i = 0; i < lm->lane_count; i++){
     Lane l = lm->lanes[i];
     printf("  id = %d\n", l.id);
     printf("  node_count = %d\n\n", l.node_count);
     fflush(stdout);
     }
     lml = lml->next;
     }
     */if (lml != NULL) {
      printf("started parsing BSMs\n");
      LaneManager* lm = lml->lane_manager;

      Vehicle *veh = ParseBsm(ind.data.contents, ind.data.length, lm, NULL);
      printf("after parse bsm\n");
      fflush(stdout);
      if (veh) {
        printf("id = %d\n", veh->id);
        printf("speed = %lg\n", veh->speed);
        printf("length = %lg\n", veh->length);
        printf("width = %lg\n", veh->width);
        printf("x = %lg\n", veh->x);
        printf("y = %lg\n", veh->y);
        printf("z = %lg\n", veh->z);
        printf("lane_id = %d\n", veh->lane_id);
        printf("heading = %lg\n", veh->heading);
        printf("height = %lg\n", veh->height);
        printf("vehicle_type = %lg\n", veh->type);
        free(veh);
      } else {
        printf("failed to free\n");
      }
    }

    sleep(1);
    printf("end of app loop\n");
  }
  return 0;
}
