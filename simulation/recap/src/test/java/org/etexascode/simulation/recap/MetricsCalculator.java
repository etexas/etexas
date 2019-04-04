/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.simulation.recap;

import java.io.PrintWriter;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;

/**
 * Drives the <b>Metrics Calculator</b> program, which calculates DSRC metrics (latency, packet
 * loss, etc.) using BSMs from the log data of multiple OBU devices.
 * 
 * @author emyers
 */
class MetricsCalculator {

    /** The time before which packets are ignored. */
    private double startTime;

    /** The time after which packets are ignored. */
    private double stopTime;

    /** The last time summary data was written. */
    private double lastWrite;

    /** The time interval between summary data writes. */
    private double writeTime;

    /** The reference longitude (dd). */
    private double latReference;

    /** The reference latitude (dd). */
    private double lonReference;

    /** The list of DSRC units. */
    private List<DsrcUnit> units;

    /** The mapping for DSRC units to transmissions. */
    private Map<DsrcUnit, DsrcMessage[]> txMap;

    /** The mapping for DSRC units to receipt counts. */
    private Map<DsrcUnit, int[]> rxMap;

    /** The mapping for vehicle IDs to DSRC units. */
    private Map<Integer, DsrcUnit> vehicleMap;

    /** The number of transmitted packets. */
    private int txPackets;

    /** The number of lost packets. */
    private int lostPackets;

    /** The metrics file writer. */
    private PrintWriter metricsWriter;

    /** The summary file writer. */
    private PrintWriter summaryWriter;

    /**
     * Creates a new <code>MetricsCalculator</code> for DSRC metrics.
     */
    private MetricsCalculator() {

        // reset the metrics counters
        txPackets = 0;
        lostPackets = 0;

        // set parameters for 8:00 AM to 12:30 PM at 10 mile / telegraph
        startTime = 1467288000.0;
        stopTime = 1467304200.0;
        lastWrite = startTime;
        writeTime = 300.0;
        latReference = 42.4730436;
        lonReference = -83.2823244;

        // create the DSRC units
        units = new ArrayList<DsrcUnit>();
        units.add(new DsrcUnit("OBU 1", Paths.get("E:", "obu1.pdml")));
        units.add(new DsrcUnit("OBU 2", Paths.get("E:", "obu2.pdml")));
        units.add(new DsrcUnit("OBU 5", Paths.get("E:", "obu5.pdml")));
        units.add(new DsrcUnit("OBU 6", Paths.get("E:", "obu6.pdml")));

        // create the transmission, receipt, and vehicle maps
        txMap = new HashMap<DsrcUnit, DsrcMessage[]>();
        rxMap = new HashMap<DsrcUnit, int[]>();
        vehicleMap = new HashMap<Integer, DsrcUnit>();
    }

    /**
     * Calculates the DSRC metrics for the current DSRC units.
     * 
     * @param metricsPath the path to the metrics output file
     * @param summaryPath the path to the summary output file
     * @throws Exception if an exception occurs during DSRC processing
     */
    private void calculate(Path metricsPath, Path summaryPath) throws Exception {

        // open the output files
        metricsWriter = new PrintWriter(metricsPath.toFile());
        metricsWriter.println("Tx Unit,Lat (dd),Lon (dd),Rx Unit,Lat (dd), Lon (dd),Tx Time (s),Rx Time (s),Interval (s),Distance (cm),Latency (s/cm)");
        summaryWriter = new PrintWriter(summaryPath.toFile());
        summaryWriter.println("Time (s),Transmissions,Potential Receipts,Actual Receipts,Packet Loss");

        // create the DSRC player
        DsrcPlayer player = new DsrcPlayer(units);
        player.addDsrcListener(new DsrcListener() {

            @Override
            public void messageReceived(DsrcMessage message) {
                processMessage(message);
            }
        });

        do {

            // play all DSRC messages
            player.play();
        }
        while (player.isPlaying());

        // close the output files
        metricsWriter.close();
        summaryWriter.close();
    }

    /**
     * Executes the <b>Metrics Calculator</b> program.
     * 
     * @param args the command line arguments
     */
    public static void main(String[] args) {

        try {

            new MetricsCalculator().calculate(
                    Paths.get("E:", "metrics.csv"),
                    Paths.get("E:", "summary.csv"));
        }
        catch (Exception exception) {

            exception.printStackTrace();
        }
    }

    /**
     * Processes a successful communication.
     * 
     * @param txMessage the transmitted DSRC message
     * @param rxMessage the received DSRC message
     */
    private void processCommunication(DsrcMessage txMessage, DsrcMessage rxMessage) {

        // get the coordinates of the transmitting unit
        DsrcUnit txUnit = (DsrcUnit)txMessage.getSource();
        double[] txCoords = UtilsLatLongConversion.convertLatLongToCentimeterOffset(
                latReference, lonReference, txUnit.getLatitude(), txUnit.getLongitude(),
                UtilsLatLongConversion.GEODETIC2D);

        // get the coordinates of the receiving unit
        DsrcUnit rxUnit = (DsrcUnit)rxMessage.getSource();
        double[] rxCoords = UtilsLatLongConversion.convertLatLongToCentimeterOffset(
                latReference, lonReference, rxUnit.getLatitude(), rxUnit.getLongitude(),
                UtilsLatLongConversion.GEODETIC2D);

        // get the transmission interval and distance
        double interval = rxMessage.getTime() - txMessage.getTime();
        double distance = Math.sqrt(Math.pow(rxCoords[0] - txCoords[0], 2)
                + Math.pow(rxCoords[1] - txCoords[1], 2));

        // write the communication information
        metricsWriter.printf("%s,%f,%f,%s,%f,%f,%f,%f,%f,%f,%f%n",
                txUnit.getName(), txUnit.getLatitude(), txUnit.getLongitude(),
                rxUnit.getName(), rxUnit.getLatitude(), rxUnit.getLongitude(),
                txMessage.getTime(), rxMessage.getTime(), interval, distance,
                interval / distance);
    }

    /**
     * Processes the next played DSRC message.
     * 
     * @param message the DSRC message to process
     */
    private void processMessage(DsrcMessage message) {

        // if the message is not in the correct time frame
        double time = message.getTime();
        if (time < startTime || time > stopTime) {

            // ignore the message
            return;
        }

        // if the message is corrupt
        if (message.isCorrupt()) {

            // ignore the message
            return;
        }

        // if the message is not a WSM
        String wsm = message.getWsm();
        if (wsm == null) {

            // ignore the message
            return;
        }

        // if the message is not a BSM
        String psid = message.getPsid();
        if (psid == null || !psid.equals(DsrcUtils.BSM_PSID)) {

            // ignore the message
            return;
        }

        // if the BSM is a transmission
        Boolean isTx = message.isTx();
        if (isTx != null && isTx) {

            // process the transmission
            processTransmission(message);
        }
        // if the BSM is a receipt
        else {

            // process the receipt
            processReceipt(message);
        }

        // if the summary interval has elapsed
        if (message.getTime() - lastWrite >= writeTime) {

            // write the summary statistics
            writeSummary(message.getTime());
            lastWrite = message.getTime();
        }
    }

    /**
     * Processes the receipt of the given DSRC message.
     * 
     * @param message the DSRC message to process
     */
    private void processReceipt(DsrcMessage message) {

        // get the BSM data from the message
        BsmData rxData = new BsmData(message.getWsm());

        // get the transmitted message and the transmitting unit
        DsrcUnit txUnit = vehicleMap.get(rxData.vehicleId);
        DsrcMessage txMessage = (txUnit == null) ? null : txMap.get(txUnit)[rxData.msgCount];

        // if a source transmission exists
        if (txMessage != null) {

            // count the receipt of the transmission
            rxMap.get(txUnit)[rxData.msgCount]--;
            processCommunication(txMessage, message);
        }
    }

    /**
     * Processes the transmission of the given DSRC message.
     * 
     * @param message the DSRC message to process
     */
    private void processTransmission(DsrcMessage message) {

        txPackets++; // increment the number of transmissions

        // get the transmitting DSRC unit
        DsrcUnit unit = (DsrcUnit)message.getSource();

        // ensure that the unit is being tracked
        if (txMap.get(unit) == null) {
            txMap.put(unit, new DsrcMessage[128]);
            rxMap.put(unit, new int[128]);
        }

        // get the BSM data from the message
        BsmData txData = new BsmData(message.getWsm());

        // ensure that the vehicle ID is mapped
        if (vehicleMap.get(txData.vehicleId) == null) {
            vehicleMap.put(txData.vehicleId, unit);
        }

        try {

            // update the position of the unit
            unit.setLatitude(txData.lat);
            unit.setLongitude(txData.lon);
        }
        catch (IllegalArgumentException exception) {

            // ignore the error and continue
        }

        // if the message count has overlapped
        DsrcMessage previous = txMap.get(unit)[txData.msgCount];
        if (previous != null) {

            // count the missing receipts of the previous transmission
            lostPackets += rxMap.get(unit)[txData.msgCount];
        }

        // set the current transmission
        txMap.get(unit)[txData.msgCount] = message;
        rxMap.get(unit)[txData.msgCount] = units.size() - 1;
    }

    /**
     * Writes the current summary data to file.
     * 
     * @param time the current message time
     */
    private void writeSummary(double time) {

        // write the summary statistics
        int potentialReceipts = txPackets * (units.size() - 1);
        int actualReceipts = potentialReceipts - lostPackets;
        double packetLoss = (double)lostPackets / (double)potentialReceipts;
        summaryWriter.printf("%f,%d,%d,%d,%f%n", time, txPackets, potentialReceipts, actualReceipts, packetLoss);
        System.out.printf("%f,%d,%d,%d,%f%n", time, txPackets, potentialReceipts, actualReceipts, packetLoss);
    }

    /**
     * Defines the information of interest in a BSM.
     * 
     * @author emyers
     */
    private static class BsmData {

        /** The vehicle ID. */
        private int vehicleId;

        /** The message count. */
        private int msgCount;

        /** The latitude (dd). */
        private double lat;

        /** The longitude (dd). */
        private double lon;

        /**
         * Creates a new <code>BsmData</code> message from the given WSM.
         * 
         * @param wsm the wave short message containing BSM data
         */
        private BsmData(String wsm) {

            // create the string builders for BSM data values
            StringBuilder countBuilder = new StringBuilder(2);
            StringBuilder idBuilder = new StringBuilder(8);
            StringBuilder latBuilder = new StringBuilder(8);
            StringBuilder longBuilder = new StringBuilder(8);

            // build the strings containing the data values of interest
            int startIndex = wsm.indexOf(DsrcUtils.BSM_PREFIX) + DsrcUtils.BSM_PREFIX.length();
            for (int i = startIndex; i < startIndex + 30; i++) {

                if (i >= startIndex && i < startIndex + 2) {

                    countBuilder.append(wsm.charAt(i));
                }
                else if (i >= startIndex + 2 && i < startIndex + 10) {

                    idBuilder.append(wsm.charAt(i));
                }
                else if (i >= startIndex + 14 && i < startIndex + 22) {

                    latBuilder.append(wsm.charAt(i));
                }
                else if (i >= startIndex + 22 && i < startIndex + 30) {

                    longBuilder.append(wsm.charAt(i));
                }
            }

            // set the values from the parsed data
            msgCount = Integer.parseInt(countBuilder.toString(), 16);
            vehicleId = (int)Long.parseLong(idBuilder.toString(), 16);
            lat = (int)Long.parseLong(latBuilder.toString(), 16) * 0.0000001;
            lon = (int)Long.parseLong(longBuilder.toString(), 16) * 0.0000001;
        }
    }
}