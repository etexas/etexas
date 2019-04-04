#ifndef NATIVE_FUNCTIONS_H
#define NATIVE_FUNCTIONS_H

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
 * File:   native-functions.h
 * Author: bbadillo
 *
 * Created on September 18, 2014, 1:51 PM
 */

#include "wave.h"

#ifdef __CPLUSPLUS
extern "C" {
#endif
// Supporting functions for wave.h
int lib_rxWSMPacket(int pid, WSMIndication *ind, int wave_device_type,
    int wave_device_blockflag);
int lib_txWSMPacket(int pid, WSMRequest *req, int wave_device_type,
    int wave_device_blockflag);
void lib_get_gps_status(GPSData *gpsdat, char *gpsadd);

#ifdef __CPLUSPLUS
}
#endif

#endif
