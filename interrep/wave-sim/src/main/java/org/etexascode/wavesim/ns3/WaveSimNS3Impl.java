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
package org.etexascode.wavesim.ns3;

import java.awt.Polygon;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.SystemUtils;
import org.etexascode.CoberturaIgnore;
import org.etexascode.datalayer.interfaces.IWaveSimDataLayer;
import org.etexascode.driver.SimDriverException;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.etexascode.interrep.topography.ITopography;
import org.etexascode.interrep.topography.ITopographyFeature;
import org.etexascode.interrep.topography.TopographyMessageType;
import org.etexascode.interrep.topography.Vector3;
import org.etexascode.wavesim.CellTower;
import org.etexascode.wavesim.CellularConfig;
import org.etexascode.wavesim.Constants;
import org.etexascode.wavesim.IWaveSim;
import org.etexascode.wavesim.PropagationLossModel;
import org.etexascode.wavesim.Rx;
import org.etexascode.wavesim.Tx;
import org.etexascode.wavesim.Tx.MessageType;
import org.etexascode.wavesim.WaveMessage;
import org.etexascode.wavesim.WaveSimLayer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Runs an NS3 script to simulate message transmissions.
 * 
 * @author janway
 * @author bmauldon
 * @author ttevendale
 */
public class WaveSimNS3Impl implements IWaveSim {

    /**
     * Logger for convenience.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(WaveSimNS3Impl.class);

    /**
     * Data layer
     */
    public final IWaveSimDataLayer data;

    /**
     * Handles the topography communications.
     */
    public final ITopography topography;

    /**
     * Configurations
     */
    public final Polygon segment;

    /**
     * The dsrc commands that will be updated with a time step's nodes and messages.
     */
    final List<String> dsrcCommands;

    /**
     * The lte commands that will be updated with a time step's nodes and messages.
     */
    final List<String> lteCommands;

    /**
     * The working directory for the process.
     */
    private String location;

    /**
     * The propagation loss model to use.
     */
    private PropagationLossModel propagationLossModel;

    /**
     * The tx num placeholder string
     */
    private static final String TX_NUM_PLACEHOLDER = "txNumPlaceholder";

    /**
     * The node list placeholder string
     */
    private static final String NODE_LIST_PLACEHOLDER = "nodeListPlaceholder";

    /**
     * The tx list placeholder string
     */
    private static final String TX_LIST_PLACEHOLDER = "txListPlaceholder";

    /**
     * Placeholder for the txNum parameter.
     */
    private static final Pattern txNumPattern = Pattern.compile(TX_NUM_PLACEHOLDER, Pattern.LITERAL);

    /**
     * Placeholder for the nodeList parameter.
     */
    private static final Pattern nodeListPattern = Pattern.compile(NODE_LIST_PLACEHOLDER, Pattern.LITERAL);

    /**
     * Placeholder for the txList parameter.
     */
    private static final Pattern txListPattern = Pattern.compile(TX_LIST_PLACEHOLDER, Pattern.LITERAL);

    /**
     * The environment for the script to run in.
     */
    private final Map<String, String> env;

    /**
     * Constructor
     * 
     * @param seg The area to get nodes for.
     * @param idl The data layer.
     * @param config NS3 configuration information.
     * @param topography The topography to use.
     * @param cellTowers The list of cell towers to be used in the simulation.
     * @param cellConfig The cellular configuration for the simulation.
     * @param propagationLossModel The propagation loss model to use.
     */
    public WaveSimNS3Impl(Polygon seg, IWaveSimDataLayer idl, Map<String, String> config, ITopography topography, List<CellTower> cellTowers, CellularConfig cellConfig,
            PropagationLossModel propagationLossModel) {
        segment = seg;
        data = idl;
        this.topography = topography;
        this.propagationLossModel = propagationLossModel;
        env = new HashMap<String, String>();
        env.put("LD_LIBRARY_PATH", ".");
        env.put("NS_LOG", "");

        dsrcCommands = new ArrayList<String>(4);
        lteCommands = new ArrayList<String>(8);

        if (SystemUtils.IS_OS_WINDOWS) {
            try {
                dsrcCommands.add(String.format("%s%s\\scratch\\wave-sim-dsrc.exe", config.get(Constants.NS3_CYGWIN_HOME), config.get(Constants.NS3_BUILD_DIR)));
                lteCommands.add(String.format("%s%s\\scratch\\wave-sim-lte.exe", config.get(Constants.NS3_CYGWIN_HOME), config.get(Constants.NS3_BUILD_DIR)));
                location = config.get(Constants.NS3_CYGWIN_HOME) + config.get(Constants.NS3_BUILD_DIR);
            }
            catch (NullPointerException e) {
                throw new SimDriverException("Error running NS3", "NS3 parameters could not be read from config file. Contact your System Administrator.");
            }
        }
        else if (SystemUtils.IS_OS_LINUX) {
            try {
                dsrcCommands.add("scratch/wave-sim-dsrc");
                lteCommands.add("scratch/wave-sim-lte");
                location = config.get(Constants.NS3_BUILD_DIR);
            }
            catch (NullPointerException e) {
                throw new SimDriverException("Error running NS3", "NS3 parameters could not be read from config file. Contact your System Administrator.");

            }
        }
        else {
            LOGGER.info("A user tried to run NS3 on an unsupported OS: " + SystemUtils.OS_NAME);
            throw new SimDriverException("Error running NS3", "Trying to run NS3 on an unsupported OS. Contact your System Administrator. ");
        }

        lteCommands.add(String.format("--cellTowerNum=%s", Integer.toString(cellTowers.size())));
        StringBuilder cellTowerSB = new StringBuilder(50 * cellTowers.size());
        int cellTowerMac = 0;
        for (CellTower cellTower : cellTowers) {
            cellTowerSB.append(String.format("%d,%f,%f,%f;", cellTowerMac, UtilsUnitConversion.convertCentimetersToMeters(cellTower.getX()),
                    UtilsUnitConversion.convertCentimetersToMeters(cellTower.getY()), UtilsUnitConversion.convertCentimetersToMeters(cellTower.getZ())));
            cellTowerMac++;
        }
        if (cellTowerSB.length() > 0) {

            lteCommands.add(String.format("--cellTowers=%s", cellTowerSB.toString()));
        }
        lteCommands.add(String.format("--lteConfigNum=%s", Integer.toString(CellularConfig.NUMBER_OF_CELLULAR_CONFIGURATIONS)));
        lteCommands.add(String.format(
                "--lteConfigs=UplinkBandwidth,%d;DownlinkBandwidth,%d;UplinkFrequency,%d;DownlinkFrequency,%d;CellTxPower,%f;CellNoiseFigure,%f;CellTowerTxPower,%f;CellTowerNoiseFigure,%f;",
                cellConfig.getUplinkBandwidth(), cellConfig.getDownlinkBandwidth(), cellConfig.getUplinkCarrierFrequency(), cellConfig.getDownlinkCarrierFrequency(), cellConfig.getCellTxPower(),
                cellConfig.getCellNoiseFigure(), cellConfig.getCellTowerTxPower(), cellConfig.getCellTowerNoiseFigure()));

        String propagationLossModelStr = null;
        if (propagationLossModel != null) {

            propagationLossModelStr = propagationLossModel.getName().toLowerCase();
        }
        else {

            propagationLossModelStr = PropagationLossModel.URBAN.getName().toLowerCase();
        }

        dsrcCommands.add(String.format("--environment=%s", propagationLossModelStr));
        lteCommands.add(String.format("--environment=%s", propagationLossModelStr));
        dsrcCommands.add(String.format("--txNum=%s", TX_NUM_PLACEHOLDER));
        lteCommands.add(String.format("--txNum=%s", TX_NUM_PLACEHOLDER));
        dsrcCommands.add(String.format("--nodes=%s", NODE_LIST_PLACEHOLDER));
        lteCommands.add(String.format("--nodes=%s", NODE_LIST_PLACEHOLDER));
        dsrcCommands.add(String.format("--txs=%s", TX_LIST_PLACEHOLDER));
        lteCommands.add(String.format("--txs=%s", TX_LIST_PLACEHOLDER));
    }

    /**
     * Gets the list of nodes from the data layer, constructs the parameters for NS3, and passes
     * them in. Then formats and places the output back in the data layer.
     * 
     * @param stepNum The step number for which to process messages.
     */
    @Override
    public void transmitMessages(int stepNum) {

        Iterable<Tx> nodes = data.getTxs(stepNum, segment);

        ArrayList<Tx> dsrcNodes = new ArrayList<Tx>();
        ArrayList<Tx> lteNodes = new ArrayList<Tx>();

        for (Tx tx : nodes) {

            if (MessageType.DSRC.equals(tx.messageType)) {

                dsrcNodes.add(tx);
            }
            else if (MessageType.CELLULAR.equals(tx.messageType)) {

                lteNodes.add(tx);
            }
        }

        Collection<Rx> rxs = new ArrayList<Rx>();
        Collection<Rx> dsrcRxs = handleNs3(stepNum, dsrcNodes, dsrcCommands);
        Collection<Rx> lteRxs = handleNs3(stepNum, lteNodes, lteCommands);

        if (dsrcRxs != null) {

            dsrcRxs = handleDSRCMaxDistance(dsrcRxs, dsrcNodes);
            rxs.addAll(dsrcRxs);
        }

        if (lteRxs != null) {

            rxs.addAll(lteRxs);
        }
        data.putMessages(stepNum, rxs);
    }

    /**
     * Runs the NS3 program based on the provided commands and nodes.
     * 
     * @param stepNum The step number for which to process messages.
     * @param nodes The nodes that might be transmitting or receiving messages.
     * @param commands The commands to run the NS3 program with.
     * @return The received messages.
     */
    private Collection<Rx> handleNs3(int stepNum, List<Tx> nodes, List<String> commands) {

        Map<Long, Integer> macToNodeListIndex = new HashMap<Long, Integer>();
        Map<Integer, Long> nodeListIndexToMac = new HashMap<Integer, Long>();
        Map<Integer, String> txIdToMsgId = new HashMap<Integer, String>();

        StringIntCombiner sic = createNodeList(nodes, macToNodeListIndex, nodeListIndexToMac);
        String txList = createTxList(nodes, macToNodeListIndex, txIdToMsgId);

        List<String> currentCommands = new ArrayList<String>(commands);

        currentCommands.set(currentCommands.size() - 3, txNumPattern.matcher(currentCommands.get(currentCommands.size() - 3)).replaceAll(Integer.toString(sic.i)));
        currentCommands.set(currentCommands.size() - 2, nodeListPattern.matcher(currentCommands.get(currentCommands.size() - 2)).replaceAll(sic.str));
        currentCommands.set(currentCommands.size() - 1, txListPattern.matcher(currentCommands.get(currentCommands.size() - 1)).replaceAll(txList));

        if ((sic.i <= 1) || txList.length() <= 0) {
            return null;
        }

        // TODO: janway - cleanup needs to happen if this fails
        String ns3Result = runNS3(currentCommands, location);

        String[] successfulTxs = ns3Result.split(";");
        Map<Long, Rx> nodeToReceivedTxIds = new HashMap<Long, Rx>();

        for (int k = 0; k < successfulTxs.length; ++k) {
            String[] parts = successfulTxs[k].split(",");
            if (parts.length != 3) {
                LOGGER.error("Received transmission was missing information. Expected: latency,txId,receiverIndex. Received: " + successfulTxs[k]);
                continue;
            }

            try {
                double latencyInSeconds = Integer.parseInt(parts[0]) / 1000.0;
                String msgId = txIdToMsgId.get(Integer.parseInt(parts[1]));
                Long receiverId = nodeListIndexToMac.get(Integer.parseInt(parts[2]));

                int latencyInSteps = (int)Math.ceil(latencyInSeconds / data.getStepSize());
                if (latencyInSteps == 0) {
                    latencyInSteps++; // minimum 1 step latency
                }
                int arrivingAtTimeStep = stepNum + latencyInSteps;

                Rx receiver = nodeToReceivedTxIds.get(receiverId);
                if (receiver == null) {
                    receiver = new Rx(receiverId);
                    nodeToReceivedTxIds.put(receiverId, receiver);
                }
                receiver.messages.put(msgId, arrivingAtTimeStep);
            }
            catch (NumberFormatException e) {
                throw new SimDriverException("Error running NS3", "Parsing error for latencyInSeconds. Check the NS3script. Contact your System Administrator.");
            }
        }

        return this.handleTopography(nodeToReceivedTxIds.values(), nodes);
    }

    /**
     * Converts a list of Txs to a String accepted by the NS3 script.
     * 
     * @param nodes The list of nodes.
     * @param macToNodeListIndex A mapping between mac addresses and indices in the string.
     * @param nodeListIndexToMac A mapping between indices in the string and mac addresses.
     * @return The nodes as a string for NS3.
     */
    public StringIntCombiner createNodeList(Iterable<Tx> nodes, Map<Long, Integer> macToNodeListIndex, Map<Integer, Long> nodeListIndexToMac) {

        int indexCounter = 0;
        StringBuilder nodeList = new StringBuilder();
        String separator = "";

        for (Tx wn : nodes) {

            if (!macToNodeListIndex.containsKey(wn.mac)) {

                // adds the cellular nodes to the end of the list
                nodeList.append(
                        String.format("%s%d,%f,%f,%f", separator, wn.mac, UtilsUnitConversion.convertCentimetersToMeters(wn.getX()), UtilsUnitConversion.convertCentimetersToMeters(wn.getY()),
                                UtilsUnitConversion.convertCentimetersToMeters(wn.getZ())));

                macToNodeListIndex.put(wn.mac, indexCounter);
                nodeListIndexToMac.put(indexCounter, wn.mac);
                indexCounter++;

                separator = ";";
            }
        }

        return new StringIntCombiner(nodeList.toString(), indexCounter);
    }

    /**
     * Converts a set of WaveMessages to a String accepted by the NS3 script.
     * 
     * @param nodes The list of nodes and their messages.
     * @param macToNodeListIndex A mapping between mac addresses and indices in the nodeList string.
     * @param txIdToMsgId A mapping between indices in the txList string and message ids.
     * @return The transmissions as a string for NS3.
     */
    public String createTxList(Iterable<Tx> nodes, Map<Long, Integer> macToNodeListIndex, Map<Integer, String> txIdToMsgId) {

        StringBuilder txList = new StringBuilder();
        int txIdCounter = 0;

        Map<Integer, List<String>> convertedTxs = new HashMap<Integer, List<String>>();
        // Convert Txs to NS3 input string format
        for (Tx wn : nodes) {

            Integer src = macToNodeListIndex.get(wn.mac);
            if (!convertedTxs.containsKey(src)) {

                convertedTxs.put(src, new LinkedList<String>());
            }
            Integer dest;
            for (WaveMessage wm : wn.outgoingMessages) {

                // set up command line parameters for NS3
                if (wm.isBroadcast()) {

                    dest = -1;
                }
                else {

                    dest = macToNodeListIndex.get(wm.destination);
                }
                // check destination node exists
                if (dest != null) {

                    convertedTxs.get(src).add(String.format("%d:%d:%d", dest, wm.size, txIdCounter));
                    txIdToMsgId.put(txIdCounter, wm.messageId);
                    txIdCounter++;
                }
            }
        }

        // Aggregate input strings into one string with proper separators
        for (Integer i = 0; i < convertedTxs.size(); ++i) {

            for (int k = 0; k < convertedTxs.get(i).size(); ++k) {

                if (k >= 1) {

                    txList.append(",");
                }
                txList.append(convertedTxs.get(i).get(k));
            }
            if (i != convertedTxs.size() - 1) {

                txList.append(";");
            }
        }

        return txList.toString();
    }

    /**
     * Takes the commands required by ProcessBuilder to run the NS3 script, and handles reading the
     * process' output so it doesn't hang.
     * 
     * @param commands The command and its parameters for ProcessBuilder to execute.
     * @param location The working directory for the process.
     * @return The string returned by the NS3 script.
     */
    @CoberturaIgnore
    public String runNS3(List<String> commands, String location) {
        String ret = "";

        ProcessBuilder processBuilder = new ProcessBuilder(commands);

        processBuilder.directory(new File(location));
        processBuilder.environment().putAll(env);

        try {
            // TODO: janway - possible SecurityException?
            Process process = processBuilder.start();

            InputStreamReader stdoutStreamReader = new InputStreamReader(process.getInputStream(), "ASCII");
            BufferedReader stdoutReader = new BufferedReader(stdoutStreamReader);
            InputStreamReader stderrStreamReader = new InputStreamReader(process.getErrorStream(), "ASCII");
            BufferedReader stderrReader = new BufferedReader(stderrStreamReader);

            ProcessReaderThread stdoutOutputThread = new ProcessReaderThread(stdoutReader, "ns3-stdout-output-thread");
            stdoutOutputThread.start();

            ProcessReaderThread stderrOutputThread = new ProcessReaderThread(stderrReader, "ns3-stderr-output-thread");
            stderrOutputThread.start();

            waitAndShutdown(stdoutOutputThread, stderrOutputThread, process);

            ret = stdoutOutputThread.getOutput();

            stdoutOutputThread.closeReader();
            stderrOutputThread.closeReader();
            process.destroy();

        }
        catch (IOException ioex) {
            throw new SimDriverException("Error running NS3", "Could not obtain output from NS3. Contact your System Administrator");

        }
        catch (InterruptedException iex) {

            throw new RuntimeException(iex);
        }

        return ret;
    }

    /**
     * Waits for the process to finish and closes everything down.
     * 
     * @param stdoutOutputThread The thread reading the standard output stream.
     * @param stderrOutputThread The thread reading the standard error stream.
     * @param process The process.
     * @throws InterruptedException Thrown if one of the threads is interrupted.
     */
    @CoberturaIgnore
    public void waitAndShutdown(ProcessReaderThread stdoutOutputThread, ProcessReaderThread stderrOutputThread, Process process) throws InterruptedException {
        int exitVal = process.waitFor();

        // close stream
        IOUtils.closeQuietly(process.getOutputStream());

        // if not a normal termination
        if (exitVal != 0) {
            LOGGER.debug("Process Exit Value: " + exitVal);
            LOGGER.error(String.format("Error running NS3 process:%n%s", stderrOutputThread.getOutput()));
            // close stream, kill subprocess
            IOUtils.closeQuietly(process.getErrorStream());
            process.destroy();

            throw new SimDriverException("Error running NS3", "The process exited with an error code.");

        }
        else {
            // close stream, kill subprocess
            IOUtils.closeQuietly(process.getErrorStream());
            process.destroy();
        }
    }

    /**
     * Handles the topography for the messages.
     * 
     * @param rxs The messages which were originally received.
     * @param nodes The nodes that were in the message simulation.
     * @return The received messages remaining after the topography was added in.
     */
    private Collection<Rx> handleTopography(Collection<Rx> rxs, Iterable<Tx> nodes) {

        if (topography == null) {

            return rxs;
        }

        Map<Long, Tx> mapOfMacToNodes = new HashMap<Long, Tx>();
        Map<String, Tx> mapOfMessageIdsToNodes = new HashMap<String, Tx>();

        for (Tx node : nodes) {

            for (WaveMessage wm : node.outgoingMessages) {

                mapOfMessageIdsToNodes.put(wm.messageId, node);
            }

            mapOfMacToNodes.put(node.mac, node);
        }

        Collection<Rx> receivedMessages = new ArrayList<Rx>();

        for (Rx rx : rxs) {

            Rx newRx = new Rx(rx.mac);
            for (String id : rx.messages.keySet()) {

                Tx source = mapOfMessageIdsToNodes.get(id);
                Tx destination = mapOfMacToNodes.get(rx.mac);

                Vector3 sourceVector = new Vector3(source.getX(), source.getY(), source.getZ());
                Vector3 destinationVector = new Vector3(destination.getX(), destination.getY(), destination.getZ());

                TopographyMessageType messageType = null;

                if (MessageType.DSRC.equals(source.messageType)) {

                    messageType = TopographyMessageType.DSRC;
                }
                else if (MessageType.CELLULAR.equals(source.messageType)) {

                    messageType = TopographyMessageType.CELLULAR;
                }

                ITopographyFeature feature = topography.getObstruction(sourceVector, destinationVector, messageType);
                if (feature == null) {

                    newRx.messages.put(id, rx.messages.get(id));
                }

            }
            if (!newRx.messages.isEmpty()) {

                receivedMessages.add(newRx);
            }
        }

        return receivedMessages;
    }

    /**
     * Handles the DSRC max distance for the messages.
     * 
     * @param rxs The messages which were originally received.
     * @param nodes The nodes that were in the message simulation.
     * @return The received messages remaining after the DSRC max distance was added in.
     */
    private Collection<Rx> handleDSRCMaxDistance(Collection<Rx> rxs, Iterable<Tx> nodes) {

        Map<Long, Tx> mapOfMacToNodes = new HashMap<Long, Tx>();
        Map<String, Tx> mapOfMessageIdsToNodes = new HashMap<String, Tx>();

        for (Tx node : nodes) {
            for (WaveMessage wm : node.outgoingMessages) {

                mapOfMessageIdsToNodes.put(wm.messageId, node);
            }

            mapOfMacToNodes.put(node.mac, node);
        }

        Collection<Rx> receivedMessages = new ArrayList<Rx>();

        for (Rx rx : rxs) {
            Rx newRx = new Rx(rx.mac);
            for (String id : rx.messages.keySet()) {

                if (UtilsCalculations.getDistance(mapOfMacToNodes.get(rx.mac), mapOfMessageIdsToNodes.get(id)) <= getMaxDSRCDistance()) {

                    newRx.messages.put(id, rx.messages.get(id));
                }
            }
            if (!newRx.messages.isEmpty()) {
                receivedMessages.add(newRx);
            }
        }
        return receivedMessages;
    }

    /**
     * Gets the max DSRC distance based on the propagation loss model. (Defaults to urban)
     * 
     * @return The max DSRC distance.
     */
    private int getMaxDSRCDistance() {

        int maxDistance = WaveSimLayer.MAX_DSRC_MESSAGE_DISTANCE_URBAN;

        if (propagationLossModel == PropagationLossModel.OPEN) {

            maxDistance = WaveSimLayer.MAX_DSRC_MESSAGE_DISTANCE_OPEN;
        }
        else if (propagationLossModel == PropagationLossModel.SUBURBAN) {

            maxDistance = WaveSimLayer.MAX_DSRC_MESSAGE_DISTANCE_SUBURBAN;
        }
        return maxDistance;
    }

    /**
     * Takes a stream and stores everything it reads.
     * 
     * @author janway
     */
    static class ProcessReaderThread extends Thread {

        /**
         * All of the data read from the source so far.
         */
        private final StringBuffer output = new StringBuffer("");

        /**
         * The stream to read from.
         */
        private BufferedReader source = null;

        /**
         * The time the thread should wait between checks for new output.
         */
        private static final int SLEEP_TIME_IN_MS = 10;

        /**
         * Constructor.
         * 
         * @param source The source.
         * @param name The name of the thread (will show in debugger).
         */
        @CoberturaIgnore
        public ProcessReaderThread(BufferedReader source, String name) {
            super(name);
            this.source = source;
        }

        /**
         * Reads in stream from a source and appends to output
         */
        @Override
        @CoberturaIgnore
        public void run() {
            try {
                String readLine;
                while (source != null) {
                    readLine = readFrom(source);
                    if (readLine != null) {
                        // TODO: janway - maybe change this to a BlockingQueue
                        // and have another thread go ahead and parse it
                        // (would require changing the c++ script -- it
                        // currently doesn't print anything until the simulation
                        // is done).
                        output.append(readLine);
                    }
                }
            }
            catch (IOException ex) {
                LOGGER.info(ex.toString());
            }
        }

        /**
         * Getter.
         * 
         * @return The output read so far.
         */
        @CoberturaIgnore
        public String getOutput() {
            return this.output.toString();

        }

        /**
         * Reads from a BufferedReader.
         * 
         * @param reader The reader to read from.
         * @return The String read.
         * @throws IOException Thrown if a read fails.
         */
        @CoberturaIgnore
        private String readFrom(BufferedReader reader) throws IOException {
            while (reader != null && !reader.ready()) {
                try {
                    Thread.sleep(SLEEP_TIME_IN_MS);
                }
                catch (InterruptedException ex) {
                    LOGGER.debug("NS3 output thread interrupted.");
                }
            }
            if (reader != null) {
                return reader.readLine();
            }
            else {
                return null;
            }
        }

        /**
         * Closes the reader.
         */
        @CoberturaIgnore
        public void closeReader() {
            try {
                source.close();
                LOGGER.debug("Closed reader.");

            }
            catch (IOException ex) {
                LOGGER.info(ex.toString());
            }
            finally {
                IOUtils.closeQuietly(source);
            }
        }
    }

    /**
     * Class for combining strings with integers as return values.
     * 
     * @author ablatt
     */
    private static class StringIntCombiner {

        /**
         * String to hold on to
         */
        final String str;

        /**
         * Integer to hold on to
         */
        final int i;

        /**
         * Constructor.
         * 
         * @param s String to hold on to
         * @param i Integer to hold on to
         */
        private StringIntCombiner(String s, int i) {
            this.str = s;
            this.i = i;
        }
    }
}
