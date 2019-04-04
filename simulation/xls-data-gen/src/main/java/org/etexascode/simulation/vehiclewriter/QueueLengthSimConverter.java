/**********************************************************************
 *** *                                                            * ***
 *** *  Copyright (c) 2011 Harmonia Holdings Group LLC            * ***
 *** *                                                            * ***
 *** * Permission is hereby granted to use, modify, copy, and     * ***
 *** * distribute this software and its documentation for any     * ***
 *** * purpose only without profit, provided that the above       * ***
 *** * Copyright Notice appears in all copies and that both the   * ***
 *** * Copyright Notice and this Permission Notice appears in     * ***
 *** * every copy of supporting documentation.  No title to nor   * ***
 *** * ownership of the software is transferred hereby.  The name * ***
 *** * of Harmonia Holdings Group LLC shall not be used in        * ***
 *** * advertising or publicity related to the distribution of    * ***
 *** * the software without specific, written, prior permission.  * ***
 *** * This software is provided as-delivered without expressed   * ***
 *** * or implied warranty.  Harmonia Holdings Group LLC          * ***
 *** * makes no representation about the suitability of this      * ***
 *** * software for any purpose and accepts no responsibility for * ***
 *** * its use.                                                   * ***
 *** *                                                            * ***
 *** ************************************************************** ***
 *** *                                                            * ***
 *** * This program is free software; you can redistribute it     * ***
 *** * and/or modify it under the terms of the GNU General Public * ***
 *** * License as published by the Free Software Foundation;      * ***
 *** * either version 2 of the License, or (at your option) any   * ***
 *** * later version.                                             * ***
 *** *                                                            * ***
 *** * This program is distributed in the hope that it will be    * ***
 *** * useful, but WITHOUT ANY WARRANTY; without even the implied * ***
 *** * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    * ***
 *** * PURPOSE.  See the GNU General Public License for more      * ***
 *** * details.                                                   * ***
 *** *                                                            * ***
 *** * You should have received a copy of the GNU General Public  * ***
 *** * License along with this program; if not, write to the Free * ***
 *** * Software Foundation, Inc., 51 Franklin Street, Fifth       * ***
 *** * Floor, Boston, MA 02110-1301, USA.                         * ***
 *** *                                                            * ***
 *** * For more information: http://www.gnu.org/licenses/gpl.html * ***
 *** *                                                            * ***
 **********************************************************************/
package org.etexascode.simulation.vehiclewriter;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

/**
 * This class contains the basic code necessary to execute a TEXAS Model simulation
 * one time step at a time. 
 *
 * @author bbadillo
 */
public class QueueLengthSimConverter {

    static FileWriter flowWriter;
    double prevSimTime = 0.0;
    private static int DSRC_PERCENTAGE = 100;
    private static int REPRUN = 1;
    private static String fileNameString;

    /**
     * The main class which starts the BasicSimulator.
     *
     * @param args the command line arguments
     */
    public static void main(String args[]) throws IOException {
        String filename = System.getProperty("filename");
        fileNameString = filename + "_qlen.csv";

        String repNumString = System.getProperty("repnum");
        if (repNumString == null) {
            REPRUN = 1;
        } else {
            int repnum = Integer.parseInt(repNumString);
            System.out.println("repnum=" + repnum);
            REPRUN = repnum;
            fileNameString = "p100r" + REPRUN + "_qlen.csv";        
        }

        DSRC_PERCENTAGE = 100;
        String dsrcNumString = System.getProperty("dsrcnum");
        if (dsrcNumString != null) {
            DSRC_PERCENTAGE = Integer.parseInt(dsrcNumString);
            System.out.println("dsrcnum=" + DSRC_PERCENTAGE);
        }

        System.out.println("Starting conversion...");
        File flowOutFile = new File(fileNameString);
//        flowWriter = new FileWriter(flowOutFile);

        FileReader reader = new FileReader(flowOutFile);
        BufferedReader in = new BufferedReader(reader);
        String line;
        while ((line = in.readLine()) != null) {
            String[] split = line.split(" ");
            float parseFloat = Float.parseFloat(split[1]);
            
            //            System.out.println("Simulated " + etexas.getCurrentTime() + " seconds...");
        }

        System.out.println("Conversion is finished.");
    }
}
