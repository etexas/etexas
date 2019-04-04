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
 * File:   native-lib.cpp
 * Author: bbadillo
 *
 * Created on September 18, 2014, 1:51 PM
 */

// NOTE: This define is used to make sure that this compilation unit is built
// as a C++ file even if the compiler does not use this non-standard symbol.
// The native-functions.h file depends on this.
#define __CPLUSPLUS

// system headers
#include <time.h> // time functions

// library headers
#include <NativeAppProtobuf.pb.h> // data model declaration

// project headers
#include "native-functions.h" // function declarations
#include "fifoqueue.h"        // queue for messages
#include "native-lib.h"       // interface functions to native agent
#include "wave.h"             // public API declarations

////////////////////////////////////////////////////////////////////////////////
// Utility Functions
////////////////////////////////////////////////////////////////////////////////

/**
 * Get only the time of day portion of the time in seconds since epoch.
 *
 * @return An integer representing time of day in the format HHMMSS
 */
int get_time(int Ttime) {
  char *token = NULL;
  char *str = NULL;
  char *temp = NULL;
  int ret_time = 0;
  time_t tme;
  tme = Ttime;

  // Interprets a value of seconds since epoch to a human-readable string.
  str = ctime(&tme);

  temp = (char *) malloc(sizeof(int));

  // Tokenize and throw away the date portion of the date-time string.
  token = strtok(str, " "); //week_day

  token = strtok(NULL, " "); //month

  token = strtok(NULL, " "); //date

  // Tokenize and keep the time portion of the date-time string.
  token = strtok(NULL, ":"); //hour
  sscanf(token, "%s", temp);
  memcpy(str, temp, sizeof(int));

  token = strtok(NULL, ":"); //min
  sscanf(token, "%s", temp);
  strcat(str, temp);

  token = strtok(NULL, " "); //sec
  sscanf(token, "%s", temp);
  strcat(str, temp);

  ret_time = atoi(str);

  free(temp);

  return ret_time;
}

/**
 * Get only the date portion of the time in seconds since epoch.
 *
 * @return An integer representing the date (without time) in the format DDMMYY
 */
int get_date(int Tdate)
{
  char *token = NULL;
  char *str = NULL;
  char *temp = NULL;
  int ret_date = 0;
  time_t date;
  int i = 0, month_num = 0, day = 0, year = 0;
  char mon[12][4] = { { "Jan" },
      { "Feb" },
      { "Mar" },
      { "Apr" },
      { "May" },
      { "Jun" },
      { "Jul" },
      { "Aug" },
      { "Sep" },
      { "Oct" },
      { "Nov" },
      { "Dec" }
  };
  date = Tdate;
  // Interprets a value of seconds since epoch to a human-readable string.
  str = ctime(&date);

  // Ignore the week_day
  temp = (char *) malloc(sizeof(int));
  token = strtok(str, " ");    //week_day

  // Tokenize and keep the date portion of the date-time string.
  token = strtok(NULL, " ");   //month
  sscanf(token, "%s", temp);
  for (i = 1; i <= 12; i++)
      {
    if (!strcmp(mon[i - 1], temp))
      month_num = i;
  }

  token = strtok(NULL, " ");   //date
  sscanf(token, "%s", temp);
  day = atoi(temp);
  //strcat(str,temp);

  token = strtok(NULL, ":");   //hour

  token = strtok(NULL, ":");   //min

  token = strtok(NULL, " ");   //sec

  token = strtok(NULL, " ");   //year
  sscanf(token, "%s", temp);
  year = atoi(temp);

  //strcat(str,temp);
  ret_date = (day * 10000) + (month_num * 100) + (year % 100);
  free(temp);
  return ret_date;
}

/**
 * Get the GPS data from the native agent and populate the GPSData structure.
 *
 * @param gpsdat out parameter for GPS data
 * @param gpsadd in parameter for the IP address of the GPS service
 */
void lib_get_gps_status(GPSData *gpsdat, char *gpsadd) {
  bool not_found = true;
  double lat;
  double lon;

  // Block on this call until a lat and long are given.
  do {
    Update();
    not_found = !GetLatitudeAndLongitude(lat, lon);
    if (not_found) {
      // NOTE: If we don't have a lat/long then use a fairly large
      // sleep value because this should only occur at initialization.
      sleep(1);
    }
  } while (not_found);

  gpsdat->actual_time = GetSimTime(); // Seconds since epoch
  gpsdat->time = get_time(gpsdat->actual_time); // Time of day in integer format HHMMSS
  gpsdat->date = get_date(gpsdat->actual_time); // Date in integer format DDMMYY
  gpsdat->local_tod = gpsdat->actual_time; // Time of day in seconds
  gpsdat->latitude = lat;
  gpsdat->longitude = lon;
}

/**
 * Receives a WSMP packet.
 *
 * @param pid The identifier for the calling process.
 * @param ind The packet.
 * @param wave_device_type A value indicating whether the device is in local or remote mode.
 * @param wave_device_block_flag A flag indicating if the device should use blocking in local mode.
 * @return A return code (0 is bad).
 */
int lib_rxWSMPacket(int pid, WSMIndication *ind, int wave_device_type,
    int wave_device_blockflag) {
  if (wave_device_type != WAVEDEVICE_LOCAL) {
    return 0;
  }

  etexas::WSMIndication *wsm = NULL;

  do {
    Update();

    wsm = (etexas::WSMIndication *) QueueTake(GetWsmQueue());

    if (wsm) {
      // TODO: bbadillo - put the src and dest info somewhere.
      long dest = wsm->dest();
      long src = wsm->src();

      //ind->chaninfo // In chaninfo.h
      ind->psid = 0;
      ind->version = 0;
      ind->txpriority = 0;
      ind->wsmps = 0;
      ind->security = 0;
      int size = wsm->size();
      if (size > HALFK) {
        size = HALFK;
      }
      ind->data.length = size;

      memcpy(ind->data.contents, wsm->data().c_str(), size);
      for (int i = 0; i < IEEE80211_ADDR_LEN; i++) {
        ind->macaddr[i] = 0;
      }
      ind->rssi = 0;

      return 1;
    }

  } while (wave_device_blockflag != 0);

  return 0;
}

/**
 * Transmits a WSMP packet.
 *
 * @param pid The identifier for the calling process.
 * @param req The packet.
 * @param wave_device_type A value indicating whether the device is in local or remote mode.
 * @param wave_device_block_flag A flag indicating if the device should use blocking in local mode.
 * @return A return code (-1 is bad).
 */
int lib_txWSMPacket(int pid, WSMRequest *req, int wave_device_type,
    int wave_device_blockflag) {
  if (wave_device_type != WAVEDEVICE_LOCAL) {
    return -1;
  }

  // Copy the WSM data into an eTEXAS model object.
  etexas::WSMIndication *wsm = new etexas::WSMIndication();
  wsm->set_data(req->data.contents);
  wsm->set_size(req->data.length);
  google::protobuf::int64 src = 0;
  google::protobuf::int64 dest = 0;
  memcpy(&dest, req->macaddr, IEEE80211_ADDR_LEN);
  memcpy(&src, req->srcmacaddr, IEEE80211_ADDR_LEN);
  wsm->set_dest(dest);
  wsm->set_src(src);

  // Use the native-lib to send to the associated agent.
  SendMessage(wsm);

  return 0;
}

/**
 * Get the current time.
 *
 * @param t if t is not a null pointer, then it is given the value of current time as well.
 */
time_t time(time_t *t) {
  if (t != NULL) {
    *t = GetSimTime();
  }
  return GetSimTime();
}
