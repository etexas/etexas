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

import static org.junit.Assert.assertTrue;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.etexascode.apps.RSEDevice;
import org.etexascode.appslayerdata.RSEDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.j2735_2016.elements.DSRCmsgID;
import org.etexascode.j2735_2016.elements.RequestedItem;
import org.etexascode.j2735_2016.elements.RequestedItem.Item;
import org.etexascode.j2735_2016.messages.BasicSafetyMessage;
import org.etexascode.j2735_2016.messages.CommonSafetyRequest;
import org.etexascode.j2735_2016.messages.MessageFrame;
import org.etexascode.wavesim.WaveMessage;
import org.junit.Before;
import org.junit.Test;

/**
 * Unit tests for the CSRProducer2016App.
 * 
 * @author ttevendale
 */
public class CSRProducer2016AppTest {

    RSEDevice device;

    @Before
    public void init() {

        device = new RSEDevice(new RSEDeviceInfo(new HashMap<Integer, ISignalManager>(), new HashMap<Integer, ILaneManager>(), new HashMap<Integer, IDetectorManager>(),
                new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));
    }

    @Test
    public void testGetAppName() {

        assertTrue(CSRProducer2016App.APP_NAME_CSR_PRODUCER_2016_APP.equals(new CSRProducer2016App().getAppName()));
    }

    @Test
    public void testInitReserved() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "true", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.RESERVED.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitItemA() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "true", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.A.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitItemB() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "false", "true", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.B.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitItemC() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "false", "false", "true", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.C.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitItemD() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "false", "false", "false", "true", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.D.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitItemE() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "false", "false", "false", "false", "true", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.E.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitItemF() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "false", "false", "false", "false", "false", "true", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.F.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitItemG() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "false", "false", "false", "false", "false", "false", "true", "false", "false", "false", "false", "false", "false", "false", "false", "false" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.G.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitItemI() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "false", "false", "false", "false", "false", "false", "false", "true", "false", "false", "false", "false", "false", "false", "false", "false" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.I.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitItemJ() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "false", "false", "false", "false", "false", "false", "false", "false", "true", "false", "false", "false", "false", "false", "false", "false" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.J.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitItemK() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "true", "false", "false", "false", "false", "false", "false" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.K.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitItemL() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "true", "false", "false", "false", "false", "false" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.L.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitItemM() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "true", "false", "false", "false", "false" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.M.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitItemN() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "true", "false", "false", "false" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.N.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitItemO() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "true", "false", "false" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.O.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitItemP() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "true", "false" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.P.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitItemQ() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "true" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.Q.equals(items[0].getEnumeration()));
    }

    @Test
    public void testInitMultipleRequests() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "false", "false", "false", "true", "false", "false", "true", "false", "false", "false", "false", "false", "false", "false", "false", "false", "true" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.C.equals(items[0].getEnumeration()));
        assertTrue(Item.F.equals(items[1].getEnumeration()));
        assertTrue(Item.Q.equals(items[2].getEnumeration()));
    }

    @Test
    public void testinitAllRequests() throws UnsupportedEncodingException {

        String[] appConfigs = new String[] { "true", "true", "true", "true", "true", "true", "true", "true", "true", "true", "true", "true", "true", "true", "true", "true", "true" };
        RequestedItem[] items = helpTestInit(appConfigs);

        assertTrue(Item.RESERVED.equals(items[0].getEnumeration()));
        assertTrue(Item.A.equals(items[1].getEnumeration()));
        assertTrue(Item.B.equals(items[2].getEnumeration()));
        assertTrue(Item.C.equals(items[3].getEnumeration()));
        assertTrue(Item.D.equals(items[4].getEnumeration()));
        assertTrue(Item.E.equals(items[5].getEnumeration()));
        assertTrue(Item.F.equals(items[6].getEnumeration()));
        assertTrue(Item.G.equals(items[7].getEnumeration()));
        assertTrue(Item.I.equals(items[8].getEnumeration()));
        assertTrue(Item.J.equals(items[9].getEnumeration()));
        assertTrue(Item.K.equals(items[10].getEnumeration()));
        assertTrue(Item.L.equals(items[11].getEnumeration()));
        assertTrue(Item.M.equals(items[12].getEnumeration()));
        assertTrue(Item.N.equals(items[13].getEnumeration()));
        assertTrue(Item.O.equals(items[14].getEnumeration()));
        assertTrue(Item.P.equals(items[15].getEnumeration()));
        assertTrue(Item.Q.equals(items[16].getEnumeration()));
    }

    @Test
    public void testPerformUpdateOneMessage() throws UnsupportedEncodingException {

        List<BasicMessage> messages = new ArrayList<BasicMessage>(1);

        String vehicleId = "11224455";
        BasicSafetyMessage bsm = new BasicSafetyMessage();
        bsm.getCoreData().getId().setValue(vehicleId);

        MessageFrame frame = new MessageFrame(DSRCmsgID.BSM, bsm.encodeUPER());
        byte[] bsmBytes = frame.encodeHexUPER().getBytes("UTF-8");

        messages.add(new DSRCMessage(bsmBytes, DSRCChannel.CH184, WaveMessage.MACBROADCAST, bsmBytes.length, 321));

        CSRProducer2016App csrProducer = new CSRProducer2016App();
        csrProducer.init(new String[] { "true", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false" });
        csrProducer.performUpdate(device, null, messages, 12345.0, null);

        List<BasicMessage> deviceMessages = device.getAppMessages();

        assertTrue(deviceMessages.size() == 1);

        BasicMessage message = deviceMessages.get(0);

        assertTrue(message.getPeerMACAddress() == 321);

        byte[] csrBytes = (byte[])message.getData();

        assertTrue(csrBytes.length == message.getSize());

        CommonSafetyRequest csr = MessageFrame.decodeCSR(new String(csrBytes));

        // 12345 seconds to rounded minute 205
        assertTrue(csr.getTimeStamp().getValue() == 205);
        assertTrue(vehicleId.equals(csr.getId().getValue()));
        assertTrue(Item.RESERVED.equals(csr.getRequests().getRequestedItemArray()[0].getEnumeration()));
    }

    @Test
    public void testPerformUpdateMultipleMessage() throws UnsupportedEncodingException {

        List<BasicMessage> messages = new ArrayList<BasicMessage>(2);

        // message 1
        String vehicleId1 = "11224455";
        BasicSafetyMessage bsm = new BasicSafetyMessage();
        bsm.getCoreData().getId().setValue(vehicleId1);

        MessageFrame frame = new MessageFrame(DSRCmsgID.BSM, bsm.encodeUPER());
        byte[] bsmBytes = frame.encodeHexUPER().getBytes("UTF-8");

        messages.add(new DSRCMessage(bsmBytes, DSRCChannel.CH184, WaveMessage.MACBROADCAST, bsmBytes.length, 1));

        // message 2
        String vehicleId2 = "fff48157";
        bsm = new BasicSafetyMessage();
        bsm.getCoreData().getId().setValue(vehicleId2);

        frame = new MessageFrame(DSRCmsgID.BSM, bsm.encodeUPER());
        bsmBytes = frame.encodeHexUPER().getBytes("UTF-8");

        messages.add(new DSRCMessage(bsmBytes, DSRCChannel.CH184, WaveMessage.MACBROADCAST, bsmBytes.length, 2));

        CSRProducer2016App csrProducer = new CSRProducer2016App();
        csrProducer.init(new String[] { "true", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false" });
        csrProducer.performUpdate(device, null, messages, 555.0, null);

        List<BasicMessage> deviceMessages = device.getAppMessages();

        assertTrue(deviceMessages.size() == 2);

        // check first message
        BasicMessage message = deviceMessages.get(0);

        assertTrue(message.getPeerMACAddress() == 1);

        byte[] csrBytes = (byte[])message.getData();

        assertTrue(csrBytes.length == message.getSize());

        CommonSafetyRequest csr = MessageFrame.decodeCSR(new String(csrBytes));

        // 555 seconds to rounded minute 9
        assertTrue(csr.getTimeStamp().getValue() == 9);
        assertTrue(vehicleId1.equals(csr.getId().getValue()));
        assertTrue(Item.RESERVED.equals(csr.getRequests().getRequestedItemArray()[0].getEnumeration()));

        // check second message
        message = deviceMessages.get(1);

        assertTrue(message.getPeerMACAddress() == 2);

        csrBytes = (byte[])message.getData();

        assertTrue(csrBytes.length == message.getSize());

        csr = MessageFrame.decodeCSR(new String(csrBytes));

        // 555 seconds to rounded minute 9
        assertTrue(csr.getTimeStamp().getValue() == 9);
        assertTrue(vehicleId2.equals(csr.getId().getValue()));
        assertTrue(Item.RESERVED.equals(csr.getRequests().getRequestedItemArray()[0].getEnumeration()));
    }

    @Test
    public void testPerformUpdateDiffMessage() throws UnsupportedEncodingException {

        List<BasicMessage> messages = new ArrayList<BasicMessage>(1);

        MessageFrame frame = new MessageFrame(DSRCmsgID.SPAT, "");
        byte[] pretendSpatBytes = frame.encodeHexUPER().getBytes("UTF-8");

        messages.add(new DSRCMessage(pretendSpatBytes, DSRCChannel.CH184, WaveMessage.MACBROADCAST, pretendSpatBytes.length, 321));

        CSRProducer2016App csrProducer = new CSRProducer2016App();
        csrProducer.init(new String[] { "true", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false" });
        csrProducer.performUpdate(device, null, messages, 0.0, null);

        List<BasicMessage> deviceMessages = device.getAppMessages();

        assertTrue(deviceMessages.size() == 0);
    }

    @Test
    public void testPerformUpdateNotByteArray() throws UnsupportedEncodingException {

        List<BasicMessage> messages = new ArrayList<BasicMessage>(1);

        String notByteArray = "someRandomStuff";
        messages.add(new DSRCMessage(notByteArray, DSRCChannel.CH184, WaveMessage.MACBROADCAST, notByteArray.length(), 321));

        CSRProducer2016App csrProducer = new CSRProducer2016App();
        csrProducer.init(new String[] { "true", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false" });
        csrProducer.performUpdate(device, null, messages, 0.0, null);

        List<BasicMessage> deviceMessages = device.getAppMessages();

        assertTrue(deviceMessages.size() == 0);
    }

    @Test
    public void testPerformUpdateCSROnlySentOnceToVehicle() throws UnsupportedEncodingException {

        List<BasicMessage> messages = new ArrayList<BasicMessage>(1);

        BasicSafetyMessage bsm = new BasicSafetyMessage();
        bsm.getCoreData().getId().setValue("110099ff");

        MessageFrame frame = new MessageFrame(DSRCmsgID.BSM, bsm.encodeUPER());
        byte[] bsmBytes = frame.encodeHexUPER().getBytes("UTF-8");

        messages.add(new DSRCMessage(bsmBytes, DSRCChannel.CH184, WaveMessage.MACBROADCAST, bsmBytes.length, 321));

        CSRProducer2016App csrProducer = new CSRProducer2016App();
        csrProducer.init(new String[] { "true", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false" });
        csrProducer.performUpdate(device, null, messages, 0.0, null);

        assertTrue(device.getAppMessages().size() == 1);

        device.getAppMessages().clear();

        csrProducer.performUpdate(device, null, messages, 1.0, null);

        assertTrue(device.getAppMessages().size() == 0);
    }

    /**
     * Helps test the init configurations by moving all the duplicated code to this method. This
     * method setups the csrProducer, gets the CSR, and then returns the requested item array.
     * 
     * @param appConfigs The app init configurations.
     * @return The requested item array.
     * @throws UnsupportedEncodingException If the encoding isn't supported.
     */
    private RequestedItem[] helpTestInit(String[] appConfigs) throws UnsupportedEncodingException {

        List<BasicMessage> messages = new ArrayList<BasicMessage>(1);

        BasicSafetyMessage bsm = new BasicSafetyMessage();
        MessageFrame frame = new MessageFrame(DSRCmsgID.BSM, bsm.encodeUPER());
        byte[] bsmBytes = frame.encodeHexUPER().getBytes("UTF-8");

        messages.add(new DSRCMessage(bsmBytes, DSRCChannel.CH184, WaveMessage.MACBROADCAST, bsmBytes.length));

        CSRProducer2016App csrProducer = new CSRProducer2016App();
        csrProducer.init(appConfigs);
        csrProducer.performUpdate(device, null, messages, 12345.0, null);

        List<CommonSafetyRequest> csrs = getCSRs(device);

        return csrs.get(0).getRequests().getRequestedItemArray();
    }

    /**
     * Gets the common safety requests from a device.
     * 
     * @param device The device to get the CSRs from.
     * @return The CSRs.
     */
    private List<CommonSafetyRequest> getCSRs(RSEDevice device) {

        List<BasicMessage> messages = device.getAppMessages();
        List<CommonSafetyRequest> csrs = new ArrayList<CommonSafetyRequest>(messages.size());

        for (BasicMessage message : messages) {

            Object data = message.getData();
            String csrBytes = new String((byte[])data);

            csrs.add(MessageFrame.decodeCSR(csrBytes));
        }

        return csrs;
    }
}
