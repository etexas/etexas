#ifndef NATIVE_LIB_H
#define NATIVE_LIB_H

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
 * File:   native-lib.h
 * Author: bbadillo
 *
 * Created on September 18, 2014, 1:51 PM
 */

// library includes
#include <NativeAppProtobuf.pb.h> // data model declaration

// project includes
#include "fifoqueue.h" // queue data structure
#include "detector.h"  // detector list data structure

void PrintMessages();
void PrintSignalData();
void PrintLaneData();
void PrintDetectorData();

Queue *GetWsmQueue();
DetectorList *GetCurrentDetectorList();
double GetSimTime();
void Update();
void SendMessage(etexas::WSMIndication *wsm);
bool GetLatitudeAndLongitude(double &lat, double &lon);
#endif
