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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.etexascode.api.BSMProducer;
import org.etexascode.api.MessageModule;
import org.etexascode.api.MessageProducer;
import org.etexascode.api.eTEXAS;
import org.etexascode.calculators.QueueLengthMOECalc;
import org.etexascode.intercom.listeners.SimDataChangeListener;
import org.etexascode.intercom.spi.InterComRxFactory;
import org.etexascode.intercom.spi.SimDataReceiver;
import org.etexascode.interrep.InterRep;
import org.etexascode.interrep.listeners.DetectorUpdateListener;
import org.etexascode.interrep.listeners.SignalUpdateListener;
import org.etexascode.interrep.listeners.VehicleUpdateListener;
import org.etexascode.j2735.BasicSafetyMessage;
import org.etexascode.nonstd.SimData;

/**
 * This class contains the basic code necessary to execute a TEXAS Model simulation
 * one time step at a time. 
 *
 * @author bbadillo
 */
public class TravelTimeSim implements SimDataChangeListener {

    QueueLengthMOECalc qCalc;
    InterRep model;
    static FileWriter travelWriter;
    static FileWriter queueLengthWriter;
    static FileWriter queueFrontWriter;
    static FileWriter queueBackWriter;
    static BufferedReader queueLengthReader;
    double prevSimTime = 0.0;
    private static int DSRC_PERCENTAGE = 100;
    private static int REPRUN = 1;
    private static String travelFileNameString;
    private static String queueFileNameString;
    private static String frontFileNameString;
    private static String backFileNameString;
    static private double prevTravelValue = 0.0;
    static private double totalQLenErr = 0.0;
    static private double totalQFrontErr = 0.0;
    static private double totalQBackErr = 0.0;
    static private int qLenErrWeight = 0;

    public TravelTimeSim() throws ClassNotFoundException {
        try {
            model = new InterRep();

            qCalc = new QueueLengthMOECalc(model);
            model.registerListener((VehicleUpdateListener) qCalc);
            model.registerListener((SignalUpdateListener) qCalc);
            model.registerListener((DetectorUpdateListener) qCalc);
            qCalc.registerLane(3);

            boolean travelNewFile = false;
            File travelOutFile = new File(travelFileNameString);
            if (!travelOutFile.exists()) {
                travelNewFile = true;
            }
            travelWriter = new FileWriter(travelOutFile, true);
            StringBuilder sb3 = new StringBuilder();
            if (travelNewFile) {
                sb3.append("DSRC Percentage,Replicate Number,Set 1,Set 2,Set 3,Set 4,Set 5,Set 6,Set 7,Set 8,Set 9,Set 10,Set 11,Set 12,Set 13,Set 14\n");
            }
            sb3.append(DSRC_PERCENTAGE);
            sb3.append(",");
            sb3.append(REPRUN);
            sb3.append(",");
            travelWriter.write(sb3.toString());
            travelWriter.flush();

            boolean queueNewFile = false;
            File queueOutFile = new File(queueFileNameString);
            if (!queueOutFile.exists()) {
                queueNewFile = true;
            }
            queueLengthWriter = new FileWriter(queueOutFile, true);
            StringBuilder sb4 = new StringBuilder();
            if (queueNewFile) {
                sb4.append("DSRC Percentage,Replicate Number,Set 1,Set 2,Set 3,Set 4,Set 5,Set 6,Set 7,Set 8,Set 9,Set 10,Set 11,Set 12,Set 13,Set 14\n");
            }
            sb4.append(DSRC_PERCENTAGE);
            sb4.append(",");
            sb4.append(REPRUN);
            sb4.append(",");
            queueLengthWriter.write(sb4.toString());
            queueLengthWriter.flush();

            boolean frontNewFile = false;
            File frontOutFile = new File(frontFileNameString);
            if (!frontOutFile.exists()) {
                frontNewFile = true;
            }
            queueFrontWriter = new FileWriter(frontOutFile, true);
            StringBuilder sb5 = new StringBuilder();
            if (frontNewFile) {
                sb5.append("DSRC Percentage,Replicate Number,Set 1,Set 2,Set 3,Set 4,Set 5,Set 6,Set 7,Set 8,Set 9,Set 10,Set 11,Set 12,Set 13,Set 14\n");
            }
            sb5.append(DSRC_PERCENTAGE);
            sb5.append(",");
            sb5.append(REPRUN);
            sb5.append(",");
            queueFrontWriter.write(sb5.toString());
            queueFrontWriter.flush();

            boolean backNewFile = false;
            File backOutFile = new File(backFileNameString);
            if (!backOutFile.exists()) {
                backNewFile = true;
            }
            queueBackWriter = new FileWriter(backOutFile, true);
            StringBuilder sb6 = new StringBuilder();
            if (backNewFile) {
                sb6.append("DSRC Percentage,Replicate Number,Set 1,Set 2,Set 3,Set 4,Set 5,Set 6,Set 7,Set 8,Set 9,Set 10,Set 11,Set 12,Set 13,Set 14\n");
            }
            sb6.append(DSRC_PERCENTAGE);
            sb6.append(",");
            sb6.append(REPRUN);
            sb6.append(",");
            queueBackWriter.write(sb6.toString());
            queueBackWriter.flush();

        } catch (IOException ex) {
            LoggerFactory.getLogger(TravelTimeSim.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    /**
     * The main class which starts the BasicSimulator.
     *
     * @param args the command line arguments
     */
    public static void main(String args[]) throws IOException, ClassNotFoundException {

        travelFileNameString = "StatTravel.csv";
        queueFileNameString = "StatQueue.csv";
        frontFileNameString = "StatFront.csv";
        backFileNameString = "StatBack.csv";

        String repNumString = System.getProperty("repnum");
        if (repNumString == null) {
            REPRUN = 1;
        } else {
            int repnum = Integer.parseInt(repNumString);
            System.out.println("repnum=" + repnum);
            REPRUN = repnum;
        }

        DSRC_PERCENTAGE = 100;
        String dsrcNumString = System.getProperty("dsrcnum");
        if (dsrcNumString != null) {
            DSRC_PERCENTAGE = Integer.parseInt(dsrcNumString);
            System.out.println("dsrcnum=" + DSRC_PERCENTAGE);
        }

        try {
            FileReader inReader = new FileReader("p100r" + REPRUN + "_qlen.dat");
            queueLengthReader = new BufferedReader(inReader);
        } catch (Exception e) {
            System.out.println("Warning: Not calculating queue length error.");
            queueLengthReader = null;
        }

        TravelTimeSim sim = new TravelTimeSim();
        SimDataReceiver simDataReceiver = InterComRxFactory.getCommunicator(SimDataReceiver.class, null);
        simDataReceiver.registerListener(sim);
        simDataReceiver.startListening();

        System.out.println("jna.library.path=" + System.getProperty("jna.library.path"));

        // Initialize the simulation
        eTEXAS etexas = new eTEXAS();
        etexas.setup();
        //Set the random DSRC assignment to be predictable.
        MessageModule module = etexas.getModule(BasicSafetyMessage.class.getName());
        if (module != null) {
            MessageProducer messageProducer = module.getMessageProducer();
            if (messageProducer != null && messageProducer instanceof BSMProducer) {
                BSMProducer bsmProducer = (BSMProducer) messageProducer;
                bsmProducer.resetRandomizer(REPRUN);
                bsmProducer.setDsrcPercentage(DSRC_PERCENTAGE);
            }
        }

        System.out.println("Starting simulation...");

        while (!etexas.isFinished()) {
            etexas.nextTimeStep();
//            System.out.println("Simulated " + etexas.getCurrentTime() + " seconds...");
        }

        StringBuilder travelString = new StringBuilder();
        travelString.append(prevTravelValue);
        travelString.append(",");
        travelWriter.write(travelString.toString());
        travelWriter.flush();

        StringBuilder qLenString = new StringBuilder();
        qLenString.append((totalQLenErr / ((double) qLenErrWeight)));
        qLenString.append(",");
        queueLengthWriter.write(qLenString.toString());
        queueLengthWriter.flush();

        StringBuilder qFrontString = new StringBuilder();
        qFrontString.append((totalQFrontErr / ((double) qLenErrWeight)));
        qFrontString.append(",");
        queueFrontWriter.write(qFrontString.toString());
        queueFrontWriter.flush();

        StringBuilder qBackString = new StringBuilder();
        qBackString.append((totalQBackErr / ((double) qLenErrWeight)));
        qBackString.append(",");
        queueBackWriter.write(qBackString.toString());
        queueBackWriter.flush();

        travelWriter.write("\n");
        travelWriter.flush();
        queueLengthWriter.write("\n");
        queueLengthWriter.flush();
        queueFrontWriter.write("\n");
        queueFrontWriter.flush();
        queueBackWriter.write("\n");
        queueBackWriter.flush();

        System.out.println("Simulation is finished.");
    }

    @Override
    public void update(SimData data) {
        try {
            double simTime = data.getSimTime();

            double actualQueueLength = 0.0;
            double actualQueueFront = 0.0;
            double actualQueueBack = 0.0;
            if (queueLengthReader != null) {
                String line = queueLengthReader.readLine();
                String[] split = line.split(" ");
                actualQueueLength = Double.parseDouble(split[1]);
                actualQueueFront = Double.parseDouble(split[2]);
                actualQueueBack = Double.parseDouble(split[3]);
            }

            /**
             * Make sure to update the model with incoming messages each time step. 
             */
            model.update();
            /**
             * Advance the time in the Queue Length calculator
             */
            qCalc.advance(simTime - prevSimTime);
            prevSimTime = simTime;
            qLenErrWeight += 1;

            double qLen = 0.0;
            String qLength = qCalc.getOutput("Q Length: 3");
            if (qLength != null) {
                qLen = Double.parseDouble(qLength);
            }
            double qLenErr = Math.abs(qLen - actualQueueLength);
            totalQLenErr += qLenErr;

            double qFront = 0.0;
            String head = qCalc.getOutput("Q Head: 3");
            if (head != null) {
                qFront = Double.parseDouble(head);
            }
            double qFrontErr = Math.abs(qFront - actualQueueFront);
            totalQFrontErr += qFrontErr;

            double qBack = 0.0;
            String projQ = qCalc.getOutput("Projected Q: 3");
            if (projQ != null) {
                qBack = Double.parseDouble(projQ);
            }
            double qBackErr = Math.abs(qBack - actualQueueBack);
            totalQBackErr += qBackErr;

            // Get the queue travel time. 
            String qTravel = qCalc.getOutput("Q Travel: 3");

            /**
             * Output the travel time data
             */
            if (qTravel != null) {
                double qTravelValue = Double.parseDouble(qTravel);
                if (qTravelValue < prevTravelValue) {
                    StringBuilder travelString = new StringBuilder();
                    travelString.append(prevTravelValue);
                    travelString.append(",");
                    travelWriter.write(travelString.toString());
                    travelWriter.flush();

                    StringBuilder qLenString = new StringBuilder();
                    qLenString.append((totalQLenErr / ((double) qLenErrWeight)));
                    qLenString.append(",");
                    queueLengthWriter.write(qLenString.toString());
                    queueLengthWriter.flush();

                    StringBuilder qFrontString = new StringBuilder();
                    qFrontString.append((totalQFrontErr / ((double) qLenErrWeight)));
                    qFrontString.append(",");
                    queueFrontWriter.write(qFrontString.toString());
                    queueFrontWriter.flush();

                    StringBuilder qBackString = new StringBuilder();
                    qBackString.append((totalQBackErr / ((double) qLenErrWeight)));
                    qBackString.append(",");
                    queueBackWriter.write(qBackString.toString());
                    queueBackWriter.flush();

                    totalQLenErr = 0.0;
                    totalQFrontErr = 0.0;
                    totalQBackErr = 0.0;
                    qLenErrWeight = 0;
                }
                prevTravelValue = qTravelValue;
            }

        } catch (IOException ex) {
            LoggerFactory.getLogger(TravelTimeSim.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
