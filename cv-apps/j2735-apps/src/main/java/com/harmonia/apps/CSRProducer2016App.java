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
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.etexascode.CoberturaIgnore;
import org.etexascode.apps.IRSEBaseApp;
import org.etexascode.apps.RSEDevice;
import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IAppName;
import org.etexascode.j2735_2016.elements.DSRCmsgID;
import org.etexascode.j2735_2016.elements.MinuteOfTheYear;
import org.etexascode.j2735_2016.elements.RequestedItem;
import org.etexascode.j2735_2016.elements.RequestedItem.Item;
import org.etexascode.j2735_2016.frames.RequestedItemList;
import org.etexascode.j2735_2016.messages.BasicSafetyMessage;
import org.etexascode.j2735_2016.messages.CommonSafetyRequest;
import org.etexascode.j2735_2016.messages.MessageFrame;

/**
 * A CSR producing Connected Vehicle Application for the 2016 j2735 specification.
 * 
 * @author ttevendale
 */
public class CSRProducer2016App implements IRSEBaseApp, IAppLifecycle, IAppName {

    /**
     * The application identifier.
     */
    public static final String APP_NAME_CSR_PRODUCER_2016_APP = "CSRProducer2016App";

    /**
     * The set of MAC addresses for the vehicles that have already been sent a CSR message
     */
    private final Set<Long> storedMac = new HashSet<Long>();

    /**
     * The indication of reserved (RequestedItem).
     */
    @AppConfigProperty(value = "false", description = "The indication of whether or not to request reserved data.")
    private boolean requestReserved;

    /**
     * The indication of Item A (RequestedItem). Requests information on the lights
     * (ExterirorLights) and light bar (LightbarInUse) data.
     */
    @AppConfigProperty(value = "false", description = "The indication of whether or not to request lights (ExteriroLights) and light bar (LightbarInUse) data.")
    private boolean requestItemA;

    /**
     * The indication of Item B (RequestedItem). Requests information on the wipers (WiperSet) data
     */
    @AppConfigProperty(value = "false", description = "The indication of whether or not to request wipers (WiperSet) data.")
    private boolean requestItemB;

    /**
     * The indication of Item C (RequestedItem). Requests information on the brake status
     * (BrakeSystemStatus) data
     */
    @AppConfigProperty(value = "false", description = "The indication of whether or not to request brake status (BrakeSystemStatus) data.")
    private boolean requestItemC;

    /**
     * The indication of Item D (RequestedItem). Requests information on the brake pressure
     * (BrakeAppliedPressure) and road friction (CoefficientOfFriction) data
     */
    @AppConfigProperty(value = "false", description = "The indication of whether or not to request brake pressure (BrakeAppliedPressure) and road friction (CoefficientOfFriction) data.")
    private boolean requestItemD;

    /**
     * The indication of Item E (RequestedItem). Requests information on the sun (SunSensor), rain
     * (RainSensor), air temperature (AmbientAirTemperature), and air pressure (AmbientAirPressure)
     * data
     */
    @AppConfigProperty(
            value = "false",
            description = "The indication of whether or not to request sun (SunSensor), rain (RainSensor), air temperature (AmbientAirTemperature), and air pressure (AmbientAirPressure) data.")
    private boolean requestItemE;

    /**
     * The indication of Item F (RequestedItem). Requests information on the steering data
     */
    @AppConfigProperty(value = "false", description = "The indication of whether or not to request steering  data.")
    private boolean requestItemF;

    /**
     * The indication of Item G (RequestedItem). Requests information on the acceleration sets data
     */
    @AppConfigProperty(value = "false", description = "The indication of whether or not to request acceleration sets data.")
    private boolean requestItemG;

    /**
     * The indication of Item I (RequestedItem). Requests information on the full postion
     * (FullPositionVector) data
     */
    @AppConfigProperty(value = "false", description = "The indication of whether or not to request full postion (FullPositionVector) data.")
    private boolean requestItemI;

    /**
     * The indication of Item J (RequestedItem). Requests information on the position 2D
     * (Position2D) data
     */
    @AppConfigProperty(value = "true", description = "The indication of whether or not to request position 2D (Position2D) data.")
    private boolean requestItemJ;

    /**
     * The indication of Item K (RequestedItem). Requests information on the position 3D
     * (Position3D) data
     */
    @AppConfigProperty(value = "false", description = "The indication of whether or not to request position 3D (Position3D) data.")
    private boolean requestItemK;

    /**
     * The indication of Item L (RequestedItem). Requests information on the speed, heading, and
     * confidence (SpeedHeadingConfidence) data
     */
    @AppConfigProperty(value = "false", description = "The indication of whether or not to request speed, heading, and confidence (SpeedHeadingConfidence) data.")
    private boolean requestItemL;

    /**
     * The indication of Item M (RequestedItem). Requests information on extra vehicle data
     */
    @AppConfigProperty(value = "false", description = "The indication of whether or not to request vehicle data.")
    private boolean requestItemM;

    /**
     * The indication of Item N (RequestedItem). Requests information on the vehicle ident
     * (VehicleIdent) data
     */
    @AppConfigProperty(value = "false", description = "The indication of whether or not to request vehicle ident (VehicleIdent) data.")
    private boolean requestItemN;

    /**
     * The indication of Item O (RequestedItem). Requests information on the weather report data
     */
    @AppConfigProperty(value = "false", description = "The indication of whether or not to request weather report data.")
    private boolean requestItemO;

    /**
     * The indication of Item P (RequestedItem). Requests information on the breadcrumbs
     * (PathHistory) data
     */
    @AppConfigProperty(value = "false", description = "The indication of whether or not to request breadcrumbs (PathHistory) data.")
    private boolean requestItemP;

    /**
     * The indication of Item Q (RequestedItem). Requests information on the GNSS status
     * (GNSSstatus) data
     */
    @AppConfigProperty(value = "false", description = "The indication of whether or not to request GNSS status (GNSSstatus) data.")
    private boolean requestItemQ;

    /**
     * Analyzes a message to find out if there's a vehicle to send a common safety request message
     * to if it hasn't already had one sent to it.
     * 
     * @param message The message to analyze.
     * @param peerMacAddress The MAC address of the message's sender.
     * @return A vehicle ID that needs a CSR sent to it or null if the message is not a BSM or the
     *         vehicle has already been sent a CSR.
     */
    private String analyzeMessage(Object message, long peerMacAddress) {

        String vehicleId = null;

        if (message instanceof byte[]) {

            String messageBytes;
            try {

                messageBytes = new String((byte[])message, "UTF-8");
            }
            catch (UnsupportedEncodingException e) {

                throw new IllegalStateException("Unsupported Encoding Exception was thrown while analyzing messages.");
            }
            if (!storedMac.contains(peerMacAddress) && MessageFrame.getDSRCMessageId(messageBytes) == DSRCmsgID.BSM) {

                BasicSafetyMessage bsm = MessageFrame.decodeBSM(messageBytes);
                storedMac.add(peerMacAddress);
                vehicleId = bsm.getCoreData().getId().getValue();
            }
        }

        return vehicleId;
    }

    /**
     * Builds a Common Safety Request message.
     * 
     * @param simTime The current simulation time.
     * @param vehicleId The vehicle ID that the message will be sent to.
     * @return The CSR message.
     */
    private byte[] buildCSR(double simTime, String vehicleId) {

        CommonSafetyRequest csr = new CommonSafetyRequest(buildRequests());
        csr.setTimeStamp(buildTimeStamp(simTime));
        csr.setId(vehicleId);

        MessageFrame frame = new MessageFrame(DSRCmsgID.CSR, csr.encodeUPER());

        byte[] csrBytes = null;

        try {

            csrBytes = frame.encodeHexUPER().getBytes("UTF-8");
        }
        catch (UnsupportedEncodingException e) {

            throw new RuntimeException("Unsupported Encoding Exception was thrown while creating CSR.");
        }

        return csrBytes;
    }

    /**
     * Builds the requests for a CSR message.
     * 
     * @return The requests.
     */
    private RequestedItemList buildRequests() {

        ArrayList<RequestedItem> itemsArrayList = new ArrayList<RequestedItem>();
        if (requestReserved) {

            itemsArrayList.add(new RequestedItem(Item.RESERVED));
        }
        if (requestItemA) {

            itemsArrayList.add(new RequestedItem(Item.A));
        }
        if (requestItemB) {

            itemsArrayList.add(new RequestedItem(Item.B));
        }
        if (requestItemC) {

            itemsArrayList.add(new RequestedItem(Item.C));
        }
        if (requestItemD) {

            itemsArrayList.add(new RequestedItem(Item.D));
        }
        if (requestItemE) {

            itemsArrayList.add(new RequestedItem(Item.E));
        }
        if (requestItemF) {

            itemsArrayList.add(new RequestedItem(Item.F));
        }
        if (requestItemG) {

            itemsArrayList.add(new RequestedItem(Item.G));
        }
        if (requestItemI) {

            itemsArrayList.add(new RequestedItem(Item.I));
        }
        if (requestItemJ) {

            itemsArrayList.add(new RequestedItem(Item.J));
        }
        if (requestItemK) {

            itemsArrayList.add(new RequestedItem(Item.K));
        }
        if (requestItemL) {

            itemsArrayList.add(new RequestedItem(Item.L));
        }
        if (requestItemM) {

            itemsArrayList.add(new RequestedItem(Item.M));
        }
        if (requestItemN) {

            itemsArrayList.add(new RequestedItem(Item.N));
        }
        if (requestItemO) {

            itemsArrayList.add(new RequestedItem(Item.O));
        }
        if (requestItemP) {

            itemsArrayList.add(new RequestedItem(Item.P));
        }
        if (requestItemQ) {

            itemsArrayList.add(new RequestedItem(Item.Q));
        }

        RequestedItemList items = new RequestedItemList(itemsArrayList.size());
        for (int i = 0; i < itemsArrayList.size(); i++) {

            items.getRequestedItemArray()[i] = itemsArrayList.get(i);
        }

        return items;
    }

    /**
     * Builds the time stamp for a CSR message. NOTE: converts seconds to minutes
     * 
     * @param simTime The simulation time to convert to a time stamp.
     * @return The time stamp.
     */
    private MinuteOfTheYear buildTimeStamp(double simTime) {

        // converts seconds to minutes
        return new MinuteOfTheYear((int)simTime / 60);
    }

    @Override
    public void init(String[] appConfigs) {

        requestReserved = Boolean.parseBoolean(appConfigs[0]);
        requestItemA = Boolean.parseBoolean(appConfigs[1]);
        requestItemB = Boolean.parseBoolean(appConfigs[2]);
        requestItemC = Boolean.parseBoolean(appConfigs[3]);
        requestItemD = Boolean.parseBoolean(appConfigs[4]);
        requestItemE = Boolean.parseBoolean(appConfigs[5]);
        requestItemF = Boolean.parseBoolean(appConfigs[6]);
        requestItemG = Boolean.parseBoolean(appConfigs[7]);
        requestItemI = Boolean.parseBoolean(appConfigs[8]);
        requestItemJ = Boolean.parseBoolean(appConfigs[9]);
        requestItemK = Boolean.parseBoolean(appConfigs[10]);
        requestItemL = Boolean.parseBoolean(appConfigs[11]);
        requestItemM = Boolean.parseBoolean(appConfigs[12]);
        requestItemN = Boolean.parseBoolean(appConfigs[13]);
        requestItemO = Boolean.parseBoolean(appConfigs[14]);
        requestItemP = Boolean.parseBoolean(appConfigs[15]);
        requestItemQ = Boolean.parseBoolean(appConfigs[16]);

    }

    @Override
    public void performUpdate(RSEDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {

        List<BasicMessage> ret = new ArrayList<BasicMessage>();
        for (BasicMessage message : receive) {

            String vehicleId = analyzeMessage(message.getData(), message.getOriginMACAddress());
            if (vehicleId != null) {

                byte[] csrBytes = buildCSR(simTime, vehicleId);
                ret.add(new DSRCMessage(csrBytes, DSRCChannel.CH184, message.getOriginMACAddress(), csrBytes.length));
            }
        }
        device.addAppMessages(ret);
    }

    @Override
    @CoberturaIgnore
    public void appShutdown(AppLogger logger) {

        logger.log(APP_NAME_CSR_PRODUCER_2016_APP, "The application has shutdown.");
    }

    @Override
    public String getAppName() {

        return APP_NAME_CSR_PRODUCER_2016_APP;
    }
}