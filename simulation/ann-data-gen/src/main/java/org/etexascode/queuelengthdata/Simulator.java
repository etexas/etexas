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
package org.etexascode.queuelengthdata;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Random;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.etexascode.api.SimproJNA;
import org.etexascode.api.eTEXAS;
import org.etexascode.intercom.listeners.SimDataChangeListener;
import org.etexascode.intercom.spi.InterComTxFactory;
import org.etexascode.intercom.spi.SimDataReceiver;
import org.etexascode.interrep.InterRep;
import org.etexascode.interrep.model.Signal;
import org.etexascode.interrep.model.SignalIndication;
import org.etexascode.interrep.model.SignalIndication.Color;
import org.etexascode.interrep.model.Vehicle;
import org.etexascode.nonstd.SimData;

/**
 * This class contains the basic code necessary to execute a TEXAS Model simulation
 * one time step at a time. 
 *
 * @author bbadillo
 */
public class Simulator implements SimDataChangeListener {

    private final static String PROJECT_NAME = "trajectory";
    private final static String TEXAS_SYS_DAT_DIR = "C:\\texas\\sys_dat";
    private final static int LANE_ID = 3;
    private final static int IN_LANE_ID = 1;
    private final static double STOPPED_SPEED_THRESHOLD = 5.0; //feet per second
    private final static double DETECTOR_POS = 750.0;
    private final static int COUNT_CAPACITY = 20;
    private static boolean splitOnRed = false;
    InterRep interRep;
    FileWriter flagNetWriter;
    FileWriter noVehWriter;
    FileWriter movingVehWriter;
    FileWriter noVehAndMovingWriter;
    FileWriter stopVehNoMovingWriter;
    FileWriter stopVehWriter;
    FileWriter bothVehWriter;
    FileWriter splitWriter;
    // An array of cumulative queue length counts that accumulate every DT. 
    int[] PLQUE = new int[SimproJNA.NIS * SimproJNA.NAL];
    int[] PMQUE = new int[SimproJNA.NIS * SimproJNA.NAL];
    int[] lastPLQUE = new int[SimproJNA.NIS * SimproJNA.NAL];
    List<Vehicle> lastVehicles = new LinkedList<Vehicle>();
    Vehicle lastDetectedVehicle = null;
    double timeSinceLastDetectedVehicle = 0.0;
    Queue<Integer> count = new LinkedList<Integer>();
    boolean redSignal = false;
    double signalCount = 0.0;
    Map<Integer, Boolean> dsrcEquipped = new HashMap<Integer, Boolean>();
    private static Random random;
    private boolean splitContexts;
    private Vehicle lastStopped = null;
    private double stopTime = 0.0;
    private int phaseNum = 0;
    private String filename;

    public Simulator(String filename, boolean splitContexts, int dsrcPercentage) throws ClassNotFoundException {
        try {
            interRep = new InterRep();
            
            this.filename = filename;
            this.splitContexts = splitContexts;

            if (!splitContexts) {
                flagNetWriter = new FileWriter(this.filename + "_etexas.dat", true);
            } else if (splitOnRed) {
                splitWriter = new FileWriter(this.filename + "_phase" + phaseNum + ".dat");
            } else {
                noVehWriter = new FileWriter(this.filename + "_noveh.dat", true);
                noVehAndMovingWriter = new FileWriter(this.filename + "_novehandmoving.dat", true);
                movingVehWriter = new FileWriter(this.filename + "_moving.dat", true);
                stopVehNoMovingWriter = new FileWriter(this.filename + "_stoppednomoving.dat", true);
                stopVehWriter = new FileWriter(this.filename + "_stopped.dat", true);
                bothVehWriter = new FileWriter(this.filename + "_bothveh.dat", true);
            }

            Arrays.fill(lastPLQUE, 0);
            for (int i = 0; i < 2000; i++) {
                dsrcEquipped.put(i, (random.nextInt(100) < dsrcPercentage));
            }

        } catch (IOException ex) {
            LoggerFactory.getLogger(Simulator.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    /**
     * The main class which starts the BasicSimulator.
     *
     * @param args the command line arguments
     */
    public static void main(String args[]) throws IOException, ClassNotFoundException {
        String filename = System.getProperty("filename");

        String splitPhase = System.getProperty("splitphase");
        if (splitPhase != null) {
            splitOnRed = true;
        }

        String repNumString = System.getProperty("repnum");
        if (repNumString == null) {
            writeSimpro(null);
            random = new Random(1);
        } else {
            int repnum = Integer.parseInt(repNumString);
            System.out.println("repnum=" + repnum);
            writeSimpro(repnum);
            random = new Random(repnum);
        }

        String randomSeedString = System.getProperty("randomseed");
        if (randomSeedString != null) {
            int randomSeed = Integer.parseInt(randomSeedString);
            random = new Random(randomSeed);
        }

        int dsrcPercentage = 100;
        String dsrcNumString = System.getProperty("dsrcnum");
        if (dsrcNumString != null) {
            dsrcPercentage = Integer.parseInt(dsrcNumString);
            System.out.println("dsrcnum=" + dsrcPercentage);
        }


        Simulator sim = new Simulator(filename, true, dsrcPercentage);
        SimDataReceiver simDataReceiver = InterComTxFactory.getCommunicator(SimDataReceiver.class, null);
        simDataReceiver.registerListener(sim);
        simDataReceiver.startListening();

        System.out.println("jna.library.path=" + System.getProperty("jna.library.path"));

        // Initialize the simulation
        eTEXAS etexas = new eTEXAS();

        System.out.println("Starting simulation...");

        // Execute the next time step in the simulation
        // until the simulation is complete
        while (!etexas.isFinished()) {
            etexas.nextTimeStep();
//            System.out.println("Simulated " + etexas.getCurrentTime() + " seconds...");
        }

        System.out.println("Simulation is finished.");
    }

    /**
     * Writes out a simpro.par file to the ProjectDirectory using the
     * ProjectName to set the file names inside the simpro.par file.
     * 
     * @throws IOException
     */
    static private void writeSimpro(Integer repnum) throws IOException {

        File simproParameters = new File("simpro.par");
        FileWriter fWriter = new FileWriter(simproParameters);
        if (repnum == null) {
            fWriter.write("I=" + PROJECT_NAME + "_sim\n");
            fWriter.write("L=" + PROJECT_NAME + "_simplst.txt\n");
            fWriter.write("STA=" + PROJECT_NAME + "_simstat\n");
            fWriter.write("T8=" + PROJECT_NAME + "_fort8\n");
            fWriter.write("T9=" + PROJECT_NAME + "_fort9\n");
            fWriter.write("PVA=" + PROJECT_NAME + "_posdat\n");
            fWriter.write("ERR=" + PROJECT_NAME + "_simerr.txt\n");
            fWriter.write("SYS_DAT=" + TEXAS_SYS_DAT_DIR + File.separator + "\n");
            fWriter.write("SSAM=" + PROJECT_NAME + "_ssam.trj\n");
            fWriter.write("PAUSEND=" + "NO\n");
        } else {
            fWriter.write("I=" + PROJECT_NAME + "_sim.rep\n");
            fWriter.write("L=" + PROJECT_NAME + "_simplst_r" + repnum + ".txt\n");
            fWriter.write("STA=" + PROJECT_NAME + "_simstat.r" + repnum + "\n");
            fWriter.write("T8=" + PROJECT_NAME + "_fort8.rep\n");
            fWriter.write("T9=" + PROJECT_NAME + "_fort9.r" + repnum + "\n");
            fWriter.write("PVA=" + PROJECT_NAME + "_posdat.r" + repnum + "\n");
            fWriter.write("ERR=" + PROJECT_NAME + "_simerr_r" + repnum + ".txt\n");
            fWriter.write("SYS_DAT=" + TEXAS_SYS_DAT_DIR + File.separator + "\n");
            fWriter.write("SSAM=" + PROJECT_NAME + "_ssam_r" + repnum + ".trj\n");
            // PARAMS.TEXAS_MODEL_NRP = 99
            if ((repnum >= 1) && (repnum <= 99)) {
                fWriter.write("REP=" + repnum + "\n");
            }
            fWriter.write("PAUSEND=" + "NO\n");
        }
        fWriter.close();
    }

    @Override
    public void update(SimData data) {

        try {

            // Get the latest information in the model. 
            interRep.update();

            // Start a buffer for an output string to hold the current data point.
            StringBuilder sb = new StringBuilder();

            /**
             * Calculate the flow of traffic at DETECTOR_POS using a moving average
             * over time
             */
            if (count.size() > COUNT_CAPACITY) {
                count.poll();
            }
            Vehicle lastDetectedVehicleCandidate = null;
            int vehicleCount = 0;
            List<Vehicle> vehicles = interRep.getVehicleManager().getVehiclesInLane(LANE_ID);
            Iterator<Vehicle> lastIter = lastVehicles.iterator();
            while (lastIter.hasNext()) {
                Vehicle lastCurrVehicle = lastIter.next();
                if (lastCurrVehicle.getDistToStopLine() > DETECTOR_POS) {
                    Iterator<Vehicle> iterator = vehicles.iterator();
                    while (iterator.hasNext()) {
                        Vehicle currVehicle = iterator.next();
                        if (currVehicle.getId() == lastCurrVehicle.getId()) {
                            if (currVehicle.getDistToStopLine() <= DETECTOR_POS) {
                                vehicleCount++;
                                lastDetectedVehicleCandidate = currVehicle;
                            }
                        }
                    }
                }
            }
            count.offer(vehicleCount);
            lastVehicles = vehicles;

            if (lastDetectedVehicleCandidate != null) {
                if (lastDetectedVehicle == null) {
                    lastDetectedVehicle = lastDetectedVehicleCandidate;
                    timeSinceLastDetectedVehicle = data.getSimTime();
                } else if (lastDetectedVehicle.getId() != lastDetectedVehicleCandidate.getId()) {
                    lastDetectedVehicle = lastDetectedVehicleCandidate;
                    timeSinceLastDetectedVehicle = data.getSimTime();
                }
            }

            int weight = 0, sum = 0;
            Iterator<Integer> countIter = count.iterator();
            while (countIter.hasNext()) {
                Integer countNum = countIter.next();
                sum += countNum;
                weight++;
            }
            if (weight == 0) {
                sb.append(0.0);
            } else {
                sb.append((double) sum / ((double) weight));
            }
            sb.append(" ");

            /**
             * Get the signal light state and time into the state in seconds
             */
            Signal signal = interRep.getSignalManager().getSignal(LANE_ID);
            SignalIndication signalIndication = signal.getSignalIndicationById(LANE_ID);
            Color color = signalIndication.getColorIndication();
            if (Color.GREEN.equals(color) || Color.YELLOW.equals(color)) {
                if (redSignal) {
                    signalCount = data.getSimTime();
                }
                redSignal = false;
                sb.append("0.0 ");
                sb.append(data.getSimTime() - signalCount + 1);
                sb.append(" ");
            } else if (Color.RED.equals(color)) {
                if (!redSignal) {
                    signalCount = data.getSimTime();
                    if (splitOnRed) {
                        phaseNum++;
                        splitWriter = new FileWriter(filename + "_phase" + phaseNum + ".dat");
                    }
                }
                redSignal = true;
                sb.append(data.getSimTime() - signalCount + 1);
                sb.append(" ");
                sb.append("0.0 ");
            } else {
                // Otherwise throw out the data point. 
                return;
            }

            /**
             * Only write the data out if the simulation time is after the startup
             * period. 
             */
            if (data.getSimTime() < 300.0 && !splitOnRed) {
                return;
            }

            /**
             * Get the last stopped and the first moving DSRC equipped vehicle. 
             */
            Vehicle lastInQueue = null;
            Vehicle firstMoving = null;
            for (int i = vehicles.size() - 1; i >= 0; i--) {
                Vehicle vehicle = vehicles.get(i);
                if (dsrcEquipped.get(vehicle.getId())) {
                    if (vehicle.getSpeed() <= STOPPED_SPEED_THRESHOLD) {
                        lastInQueue = vehicle;
                        break;
                    }
                    firstMoving = vehicle;
                }
            }

            // Output the values of the last stopped vehicle
            if (lastInQueue != null) {

                //Also, while we're at it, calculate the stop time. 
                if (lastStopped == null || (lastStopped.getId() != lastInQueue.getId())) {
                    lastStopped = lastInQueue;
                    stopTime = data.getSimTime();
                }

                sb.append(lastInQueue.getDistToStopLine());
                sb.append(" ");
            } else {
                //Also, while we're at it, calculate the stop time. 
                lastStopped = null;
                stopTime = data.getSimTime();

                sb.append("0.0 ");
            }


            if (firstMoving != null) {
                sb.append(firstMoving.getDistToStopLine());
                sb.append(" ");
                sb.append(firstMoving.getSpeed());
                sb.append(" ");
            } else {
                sb.append("0.0 0.0 ");
            }

            /**
             *  Write the flag for whether last stopped and first moving could be obtained. 
             */
            if (lastStopped == null && firstMoving == null) {
                sb.append("0.0 ");
            } else if (firstMoving == null) {
                sb.append("0.25 ");
            } else if (lastStopped == null) {
                sb.append("0.5 ");
            } else {
                sb.append("1.0 ");
            }

            /**
             * Output the time in seconds that the first moving vehicle has been stopped. 
             */
            if (lastStopped != null) {
                sb.append(data.getSimTime() - stopTime);
            } else {
                sb.append("0.0");
            }
            sb.append(" ");

            /**
             * Output the speed and time since the last detected vehicle. 
             */
            if (lastDetectedVehicle != null) {
                sb.append(lastDetectedVehicle.getSpeed());
                sb.append(" ");
                sb.append(data.getSimTime() - timeSinceLastDetectedVehicle);
                sb.append(" ");
            } else {
                sb.append("0.0 ");
                sb.append(data.getSimTime() - timeSinceLastDetectedVehicle);
                sb.append(" ");
            }

            /**
             * Output the known queue length on a new line. 
             */
            int queueLen = 0;
            Double queueFront = null;
            Double queueBack = null;
            boolean foundFirstStopped = false;
            for (int i = vehicles.size() - 1; i >= 0; i--) {
                Vehicle vehicle = vehicles.get(i);
                if (vehicle.getSpeed() <= STOPPED_SPEED_THRESHOLD) {
                    if (!foundFirstStopped) {
                        queueLen = i + 1;
                        foundFirstStopped = true;
                    }

                    if ((queueFront == null) || (queueFront > vehicle.getDistToStopLine())) {
                        queueFront = vehicle.getDistToStopLine();
                    }
                    if ((queueBack == null) || (queueBack < vehicle.getDistToStopLine())) {
                        queueBack = vehicle.getDistToStopLine();
                    }
                }
            }
            sb.append("\n");
            sb.append(queueLen);
            sb.append(" ");
            if (queueFront != null) {
                sb.append(queueFront);
            } else {
                sb.append("-1.0");
            }
            sb.append(" ");
            if (queueBack != null) {
                sb.append(queueBack);
            } else {
                sb.append("-1.0");
            }
            sb.append(" ");
            if (queueBack == null || queueFront == null) {
                sb.append("0.0");
            } else {
                sb.append("1.0");
            }
            sb.append("\n");

            // Print the current queue length statistics. 
//            System.out.print("Queue Len: " + queueLen + "\n");

            // Write the data point to file. 
            if (!splitContexts) {
                flagNetWriter.write(sb.toString());
                flagNetWriter.flush();
            } else if (splitOnRed) {
                splitWriter.write(sb.toString());
                splitWriter.flush();
            } else {
                if (lastStopped == null && firstMoving == null) {
                    noVehWriter.write(sb.toString());
                    noVehWriter.flush();
                    noVehAndMovingWriter.write(sb.toString());
                    noVehAndMovingWriter.flush();
                } else if (firstMoving == null) {
                    stopVehNoMovingWriter.write(sb.toString());
                    stopVehNoMovingWriter.flush();
                    stopVehWriter.write(sb.toString());
                    stopVehWriter.flush();
                } else if (lastStopped == null) {
                    movingVehWriter.write(sb.toString());
                    movingVehWriter.flush();
                    noVehAndMovingWriter.write(sb.toString());
                    noVehAndMovingWriter.flush();
                } else {
                    bothVehWriter.write(sb.toString());
                    bothVehWriter.flush();
                    stopVehWriter.write(sb.toString());
                    stopVehWriter.flush();
                }
            }

        } catch (IOException ex) {
            LoggerFactory.getLogger(Simulator.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
//    /**
//     * This method outputs the distance of vehicles with each column specifying a vehicle. 
//     * 
//     * @param data 
//     */
//    @Override
//    public void simDataChanged(SimData data) {
//        try {
//            // Get the latest information in the model. 
//            model.update();
//
//            StringBuilder sb = new StringBuilder();
//            // Get the Traffic flow
//            if (count.size() > COUNT_CAPACITY) {
//                count.poll();
//            }
//            int vehicleCount = 0;
//            List<Vehicle> vehicles = model.getVehicleManager().getVehiclesInLane(LANE_ID);
//            Iterator<Vehicle> lastIter = lastVehicles.iterator();
//            while (lastIter.hasNext()) {
//                Vehicle lastCurrVehicle = lastIter.next();
//                if (lastCurrVehicle.getDistToStopLine() > DETECTOR_POS) {
//                    Iterator<Vehicle> iterator = vehicles.iterator();
//                    while (iterator.hasNext()) {
//                        Vehicle currVehicle = iterator.next();
//                        if (currVehicle.getId() == lastCurrVehicle.getId()) {
//                            if (currVehicle.getDistToStopLine() <= DETECTOR_POS) {
//                                vehicleCount++;
//                            }
//                        }
//                    }
//                }
//            }
//            count.offer(vehicleCount);
//            lastVehicles = vehicles;
//
//            if (data.getSimTime() < 300.0) {
//                return;
//            }
//
//            int weight = 0, sum = 0;
//            Iterator<Integer> countIter = count.iterator();
//            while (countIter.hasNext()) {
//                Integer countNum = countIter.next();
//                sum += countNum;
//                weight++;
//            }
//            if (weight == 0) {
//                sb.append(0.0);
//            } else {
//                sb.append((double) sum / ((double) weight));
//            }
//            sb.append(" ");
//            // Get the signal light state for the lane
//            Signal signal = model.getSignalManager().getSignal(LANE_ID);
//            SignalIndication signalIndication = signal.getSignalIndicationById(LANE_ID);
//            Color color = signalIndication.getColorIndication();
//            if (Color.GREEN.equals(color)) {
//                sb.append(0.0);
//                sb.append(" ");
//            } else if (Color.YELLOW.equals(color)) {
//                sb.append(0.5);
//                sb.append(" ");
//            } else if (Color.RED.equals(color)) {
//                sb.append(1.0);
//                sb.append(" ");
//            } else {
//                // Otherwise throw out the data point. 
//                return;
//            }
//
//            int queueLen = 0;
//            // Get the last stopped and first moving DSRC vehicles
//            Vehicle lastStopped = null;
//            Vehicle firstMoving = null;
//            for(int i = vehicles.size()-1;i >=0;i--) {
//                Vehicle vehicle = vehicles.get(i);
//                if (vehicle.getSpeedMPH() <= STOPPED_SPEED_THRESHOLD) {
//                    lastStopped = vehicle;
//                    queueLen = i+1;
//                    break;
//                }
//                firstMoving = vehicle;
//            }
//
//            // Output the values of the last stopped vehicle
//            if (lastStopped != null) {
//                sb.append(lastStopped.getDistToStopLine());
//                sb.append(" ");
////                sb.append(lastStopped.getSpeedMPH());
////                sb.append(" ");
////                sb.append(model.getVehicleManager().getVehicleAcceleration(lastStopped.getId()));
////                sb.append(" ");
////                sb.append(lastStopped.getLength());
////                sb.append(" ");
//            } else {
//                sb.append("0.0 ");
//            }
//
//            if (firstMoving != null) {
//                sb.append(firstMoving.getDistToStopLine());
//                sb.append(" ");
//                sb.append(firstMoving.getSpeedMPH());
//                sb.append(" ");
////                sb.append(model.getVehicleManager().getVehicleAcceleration(firstMoving.getId()));
////                sb.append(" ");
////                sb.append(firstMoving.getLength());
////                sb.append(" ");
//            } else {
//                sb.append("0.0 0.0 ");
//            }
//
//            // Write the flag for whether last stopped and first moving could be obtained. 
//            if (lastStopped == null && firstMoving == null) {
//                sb.append("0.0 ");
//            } else if (firstMoving == null) {
//                sb.append("0.25 ");
//            } else if (lastStopped == null) {
//                sb.append("0.5 ");
//            } else {
//                sb.append("1.0 ");
//            }
//
////            // Output the known queue length on a new line. 
////            sb.append("\n");
////            // Get the queue length statistics from TEXAS. 
////            SimproInterface.INSTANCE.getQueueLengths(PLQUE, PMQUE);
////            // Print the current queue length statistics. 
////            sb.append((PLQUE[IN_LANE_ID] - lastPLQUE[IN_LANE_ID]));
////            sb.append("\n");
////
////            for (int x = 0; x < PLQUE.length; x++) {
////                System.out.print((PLQUE[x] - lastPLQUE[x]) + " ");
////
////                lastPLQUE[x] = PLQUE[x];
////            }
////            System.out.print("\n");
//            // Output the known queue length on a new line. 
//            sb.append("\n");
//            // Get the queue length statistics from TEXAS. 
//            // Print the current queue length statistics. 
//            sb.append(queueLen);
//            sb.append("\n");
//
//            System.out.print("Queue Len: " + queueLen + "\n");
//
//            flagNetWriter.write(sb.toString());
//            flagNetWriter.flush();
//
//        } catch (IOException ex) {
//            LoggerFactory.getLogger(Simulator.class.getName()).log(Level.SEVERE, null, ex);
//        }
//    }
}
