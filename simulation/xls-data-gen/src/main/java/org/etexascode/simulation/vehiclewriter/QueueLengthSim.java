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

import java.io.File;
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
public class QueueLengthSim implements SimDataChangeListener {

    QueueLengthMOECalc qCalc;
    InterRep model;
    static FileWriter flowWriter;
    double prevSimTime = 0.0;
    private static int DSRC_PERCENTAGE = 100;
    private static int REPRUN = 1;
    private static String fileNameString;

    public QueueLengthSim() throws ClassNotFoundException {
        try {
            model = new InterRep();

            qCalc = new QueueLengthMOECalc(model);
            model.registerListener((VehicleUpdateListener) qCalc);
            model.registerListener((SignalUpdateListener) qCalc);
            model.registerListener((DetectorUpdateListener) qCalc);
            qCalc.registerLane(3);

            File flowOutFile = new File(fileNameString);
            flowWriter = new FileWriter(flowOutFile);

        } catch (IOException ex) {
            LoggerFactory.getLogger(QueueLengthSim.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    /**
     * The main class which starts the BasicSimulator.
     *
     * @param args the command line arguments
     */
    public static void main(String args[]) throws IOException, ClassNotFoundException {
        String filename = System.getProperty("filename");
        fileNameString = filename + "_qlen.dat";

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

        QueueLengthSim sim = new QueueLengthSim();
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

        System.out.println("Simulation is finished.");
    }

    @Override
    public void update(SimData data) {
        try {
            double simTime = data.getSimTime();

            /**
             * Make sure to update the model with incoming messages each time step. 
             */
            model.update();
            /**
             * Advance the time in the Queue Length calculator
             */
            qCalc.advance(simTime - prevSimTime);
            prevSimTime = simTime;
            StringBuilder flowString = new StringBuilder();
            flowString.append(simTime);
            flowString.append(" ");
            String qLength = qCalc.getOutput("Q Length: 3");
            if (qLength != null) {
                flowString.append(qLength);
            } else {
                flowString.append(0.0);
            }
            flowString.append(" ");
            String head = qCalc.getOutput("Q Head: 3");
            if (head != null) {
                flowString.append(head);
            } else {
                flowString.append(0.0);
            }
            flowString.append(" ");
            String projQ = qCalc.getOutput("Projected Q: 3");
            if (projQ != null) {
                flowString.append(projQ);
            } else {
                flowString.append(0.0);
            }
            flowString.append("\n");
            flowWriter.write(flowString.toString());
            flowWriter.flush();

        } catch (IOException ex) {
            LoggerFactory.getLogger(QueueLengthSim.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
