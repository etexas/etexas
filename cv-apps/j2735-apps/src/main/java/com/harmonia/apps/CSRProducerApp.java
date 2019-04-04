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
package com.harmonia.apps;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.codec.CharEncoding;
import org.etexascode.apps.IRSEBaseApp;
import org.etexascode.apps.MsgByteCount;
import org.etexascode.apps.RSEDevice;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.FormattedDSRCMessage;
import org.etexascode.devicedata.IAppName;
import org.etexascode.j2735.BasicSafetyMessage;
import org.etexascode.j2735.BasicSafetyMessageVerbose;
import org.etexascode.j2735.CommonSafetyRequest;
import org.etexascode.j2735.VehicleStatus;
import org.etexascode.j2735.VehicleStatus.VehicleData;
import org.etexascode.j2735.util.DSRCMessageID;
import org.etexascode.nonstd.Message;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A CSR producing Connected Vehicle App.
 * 
 * @author jrutherford
 * @author bmauldon
 */
public class CSRProducerApp implements IRSEBaseApp, IAppName {

    /**
     * The application identifier.
     */
    public static final String APP_NAME_CSR_PRODUCER_APP = "CSRProducerApp";

    /** Logger for convenience. */
    private static final Logger LOGGER = LoggerFactory.getLogger(CSRProducerApp.class.getName());

    /** Holds the BSM data for new vehicle MAC's. */
    private final Map<Long, VehicleData> bsmData = new HashMap<Long, VehicleData>();

    /**
     * The set of mac addresses for the vehicles
     */
    private final Set<Long> storedMac = new HashSet<Long>();

    /**
     * The size of the message in bytes
     */
    private int size;

    /**
     * Gets the CSR Apps Map of stores BSM data.
     * 
     * @return The map of BSM data.
     */
    public Map<Long, VehicleData> getBsmData() {
        return bsmData;
    }

    /**
     * Analyzes an individual WSM Indication.
     * 
     * @param msg The indication data.
     * @param peerMacAddress The indication MAC.
     * @return A WSMRequest if needed or null.
     */
    public Object analyzeMessage(Object msg, long peerMacAddress) {
        // Initialize size to 0.
        size = 0;

        // Get the data from the message.
        byte[] blob;
        byte[] vehicleID;
        VehicleStatus vs;
        Message bsm;
        if (msg instanceof BasicSafetyMessage) {
            bsm = (BasicSafetyMessage)msg;
            blob = ((BasicSafetyMessage)msg).getBlob1();
            vehicleID = Arrays.copyOfRange(blob, 1, 5);
            vs = ((BasicSafetyMessage)msg).getStatus();
        }
        else if (msg instanceof BasicSafetyMessageVerbose) {
            bsm = (BasicSafetyMessageVerbose)msg;
            vehicleID = ((BasicSafetyMessageVerbose)msg).getId();
            vs = ((BasicSafetyMessageVerbose)msg).getStatus();
        }
        else {
            // Want BSM or BSMV only.
            return null;
        }

        if (vs == null) {
            if (!storedMac.contains(peerMacAddress)) {
                storedMac.add(peerMacAddress);
                // BSM Part 1 means send a request to get the part 2 data
                // back to the device.
                // return createCSRMessage(mac, vehicleID);
                try {
                    return getCommonSafetyRequest(vehicleID);
                }
                catch (UnsupportedEncodingException e) {
                    throw new RuntimeException(e);
                }
            }
        }
        else {
            // BSM Part 2 means the request above was answered and
            // we need to store the data it returns.
            storeBSMPart2Data(peerMacAddress, bsm);
        }

        return null;
    }

    /**
     * Creates a WSM Request containing a CSR message.
     * 
     * @param mac The MAC address to whom the message should be sent.
     * @param vehicleID The vehicle ID.
     * @return The WSM Request.
     */
    /*
     * WSMRequest createCSRMessage(long mac, byte vehicleID[]) { if (!bsmData.containsKey(mac)) {
     * try { CommonSafetyRequest csr = getCommonSafetyRequest(vehicleID); WSMRequest request = new
     * WSMRequest(); request.setData(csr); request.setPeerMACAddress(mac); return request; } catch
     * (UnsupportedEncodingException ex) { getAppLogger().log("Error", ex.toString()); } } return
     * null; }
     */
    /**
     * Stores the BSM part 2 data.
     * 
     * @param mac The MAC address of the vehicle to store data for.
     * @param bsm The BSM or BSMV message.
     */
    private void storeBSMPart2Data(long mac, Message bsm) {
        if (!bsmData.containsKey(mac)) {
            VehicleData data = null;
            if (bsm instanceof BasicSafetyMessage) {
                data = ((BasicSafetyMessage)bsm).getStatus().getVehicleData();
            }
            else if (bsm instanceof BasicSafetyMessageVerbose) {
                data = ((BasicSafetyMessageVerbose)bsm).getStatus().getVehicleData();
            }

            bsmData.put(mac, data);
            LOGGER.info("Data stored for device {0}.", Long.toHexString(mac));
        }
    }

    /**
     * Gets a common safety request for the given vehicle.
     * 
     * @param vehicleID The vehicle ID to set the request for.
     * @return A CommonSafetyRequest representing the given vehicle.
     * @throws UnsupportedEncodingException If the common safety request cannot be retrieved.
     */
    public CommonSafetyRequest getCommonSafetyRequest(byte[] vehicleID) throws UnsupportedEncodingException {
        CommonSafetyRequest csr = new CommonSafetyRequest();

        csr.setId(vehicleID);

        // increment msg byte size
        size += (int)UtilsProducer.increment(MsgByteCount.TEMPORARYID);

        csr.setMsgCnt((short)1);

        // increment msg byte size
        size += (int)UtilsProducer.increment(MsgByteCount.MSGCOUNT);

        csr.setMsgID(DSRCMessageID.MSG_ID_CSR);
        // increment msg byte size
        size += (int)UtilsProducer.increment(MsgByteCount.DSRCMSGID);

        csr.getRequests().add(new String(new byte[] { (byte)0xD }, CharEncoding.UTF_8));

        // increment msg byte size
        size += (int)UtilsProducer.increment(MsgByteCount.REQUESTEDITEM);
        return csr;
    }

    @Override
    public void performUpdate(RSEDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {

        List<BasicMessage> ret = new ArrayList<BasicMessage>();
        for (BasicMessage ind : receive) {
            if (ind instanceof FormattedDSRCMessage) {
                FormattedDSRCMessage message = (FormattedDSRCMessage)ind;
                Object o = analyzeMessage(message.getFormattedData(), message.getPeerMACAddress());
                if (o != null) {
                    ret.add(new DSRCMessage(o, DSRCChannel.CH184, message.getPeerMACAddress(), size));
                }
            }
        }
        device.addAppMessages(ret);
    }

    @Override
    public String getAppName() {
        return APP_NAME_CSR_PRODUCER_APP;
    }

}