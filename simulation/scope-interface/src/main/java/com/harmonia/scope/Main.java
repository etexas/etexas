/**
 * *****************************************************************************
 * Copyright (c) 2014 Harmonia Holdings Group LLC and others. All rights
 * reserved. This program and the accompanying materials are made available
 * under the terms of the Harmonia Partner License v1.0 which accompanies this
 * distribution, and is available at http://www.harmonia.com/legal/hpl-v10.html
 *
 * Contributors: Harmonia Holdings Group LLC - initial API and implementation
 * *****************************************************************************
 */
package com.harmonia.scope;

import java.io.IOException;

/**
 *
 * NOTE: Mapping detector actuation channels to phases: SCOPE has an internal
 * mapping capability that is not exposed to the user interface. In order to map
 * channels to phases, a table must be modified inside of SCOPE and SCOPE must
 * be recompiled. The Detector_Phase_Map array can be found in the file called
 * "scope.ads". It is here for convenience.
 *
 * Detector_Phase_Map : constant array(1 ..
 * Utilities.Number_Of_Possible_Detectors) of Phase_Type := (1 => 1, 2 => 2, 3
 * => 3, 4 => 4, 5 => 5, 6 => 6, 7 => 7, 8 => 8, 9 => 1, 10 => 2, 11 => 3, 12 =>
 * 4, 13 => 5, 14 => 6, 15 => 7, 16 => 8, 17 => 1, 18 => 2, 19 => 3, 20 => 4, 21
 * => 5, 22 => 6, 23 => 7, 24 => 8, 25 => 1, 26 => 2, 27 => 3, 28 => 4, 29 => 5,
 * 30 => 6, 31 => 7, 32 => 8, 33 => 1, 34 => 2, 35 => 3, 36 => 4, 37 => 5, 38 =>
 * 6, 39 => 7, 40 => 8, 41 => 1, 42 => 2, 43 => 3, 44 => 4, 45 => 5, 46 => 6, 47
 * => 7, 48 => 8, 49 => 1, 50 => 2, 51 => 3, 52 => 4, 53 => 5, 54 => 6, 55 => 7,
 * 56 => 8, 57 => 1, 58 => 2, 59 => 3, 60 => 4, 61 => 5, 62 => 6, 63 => 7, 64 =>
 * 8);
 *
 * @author bbadillo
 */
public class Main {

	final static int SCOPE_PRIMARY_DISPLAY_PORT = 5878;

	/**
	 * @param args the command line arguments
	 */
	public static void main(String[] args) throws IOException, InterruptedException {

//		components.Main.IOdata = new DataManager(true);
//		OverridePanelMainConsole oldFormat = new OverridePanelMainConsole(new GridBagLayout());
//		components.Main.oldFormat = oldFormat;
//		components.Main.main(args);
//		components.Main.IOdata.startIO();
//		oldFormat.startSocket();
		GUI2PrimaryData data = new GUI2PrimaryData();

		System.out.println("Starting");
		data.start();
		System.out.println("Started");

//		InputDataThread idt = new InputDataThread(components.Main.IOdata);
//		Thread thread = new Thread(idt);
//		thread.start();
		for (int i = 0; i < 500; i++) {
			data.receiveData();
			if (i == 10) {
				data.actuate();
			}
		}

		System.out.println("Stopping");
		data.stop();
		System.out.println("Stopped");
	}

}
