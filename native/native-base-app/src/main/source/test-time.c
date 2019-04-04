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
#include <time.h>

// libwave headers
#include "wave.h"

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

  WSMIndication ind;
  int ret_int = 0, i;

  while (1) {

    printf("just before rxWSMPacket\n");
    fflush(stdout);

    ret_int = rxWSMPacket(0, &ind);

    printf("\nTime = %1d\n", time(NULL));
    fflush(stdout);

    sleep(1);
  }
  return 0;
}
