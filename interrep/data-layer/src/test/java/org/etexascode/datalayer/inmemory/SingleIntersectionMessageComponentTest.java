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
package org.etexascode.datalayer.inmemory;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.appslayerdata.AppLayerOutput;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.wavesim.Rx;
import org.etexascode.wavesim.Tx;
import org.etexascode.wavesim.Tx.MessageType;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class SingleIntersectionMessageComponentTest {

    SingleIntersectionMessageComponent simc = null;

    long testMac = 0;

    long testMacOrig = 0;

    BasicMessage mr = null;

    BasicMessage mr2 = null;

    String mrConstStr = null;

    List<AppLayerOutput> appLayerOutputs = null;

    List<BasicMessage> inputMesses = null;

    Map<Integer, Map<Long, List<BasicMessage>>> testAppInputs = null;

    Map<Long, List<BasicMessage>> testAppInputStep = null;

    int goodInsertId = 8;

    List<Rx> waveSimOutput = null;

    Map<String, BasicMessage> waveSimOutputMessages = null;

    Map<Integer, Map<Long, List<BasicMessage>>> expectedWaveSimOutput = null;

    @Before
    public void setup() {
        simc = new SingleIntersectionMessageComponent();
        testMac = 1729;
        testMacOrig = 42;

        int testSize = 39;
        mrConstStr = "const str";
        mr = new DSRCMessage(mrConstStr, DSRCChannel.CH184, testMac, testSize);
        mr2 = new DSRCMessage(mrConstStr + "2", DSRCChannel.CH184, testMacOrig, testSize);

        mr.setOriginMACAddress(testMacOrig);

        appLayerOutputs = new ArrayList<AppLayerOutput>(1);
        inputMesses = new ArrayList<BasicMessage>(1);
        inputMesses.add(mr);
        String str = "";
        long num = (long)0;
        DistanceImpl location = new DistanceImpl(0, 0, 0);
        AppLayerOutput alo = new AppLayerOutput(str, num, location);
        alo.setMessageType(MessageType.DSRC);
        alo.addMessagesList(inputMesses);
        appLayerOutputs.add(alo);

        testAppInputs = new HashMap<Integer, Map<Long, List<BasicMessage>>>();
        testAppInputStep = new HashMap<Long, List<BasicMessage>>();
        testAppInputStep.put(87l, null);
        testAppInputs.put(goodInsertId, testAppInputStep);

        waveSimOutput = new ArrayList<Rx>(3);
        Rx r1 = new Rx(testMacOrig);
        r1.messages.put(mrConstStr, 1);
        waveSimOutput.add(r1);
        r1 = new Rx(testMacOrig);
        r1.messages.put(mrConstStr + "2", 1);
        waveSimOutput.add(r1);
        r1 = new Rx(testMac);
        r1.messages.put(mrConstStr, 1);
        waveSimOutput.add(r1);

        waveSimOutputMessages = new HashMap<String, BasicMessage>();
        waveSimOutputMessages.put(mrConstStr, mr);
        waveSimOutputMessages.put(mrConstStr + "2", mr2);

        expectedWaveSimOutput = new HashMap<Integer, Map<Long, List<BasicMessage>>>();
        List<BasicMessage> l = new ArrayList<BasicMessage>(2);
        List<BasicMessage> l2 = new ArrayList<BasicMessage>(1);

        l.add(new DSRCMessage(mrConstStr, DSRCChannel.CH184, testMacOrig));
        l.add(new DSRCMessage(mrConstStr + "2", DSRCChannel.CH184, testMacOrig));
        l2.add(new DSRCMessage(mrConstStr, DSRCChannel.CH184, testMac));

        Map<Long, List<BasicMessage>> ml = new HashMap<Long, List<BasicMessage>>();
        ml.put(testMacOrig, l);
        ml.put(testMac, l2);
        expectedWaveSimOutput.put(1, ml);
    }

    @After
    public void teardown() {
        mrConstStr = null;
        simc = null;
        testMac = 0;
        testMacOrig = 0;
        mr = null;
        appLayerOutputs = null;
        inputMesses = null;
        testAppInputs = null;
        testAppInputStep = null;
        waveSimOutput = null;
        waveSimOutputMessages = null;
        expectedWaveSimOutput = null;
    }

    @Test
    public void testConstructor() {
        SingleIntersectionMessageComponent simc = new SingleIntersectionMessageComponent();
        assertTrue(simc instanceof SingleIntersectionMessageComponent);
    }

    @Test
    public void testPutAppLayerOutputs() {
        long start = simc.messId;
        simc.putAppLayerOutputs(0, appLayerOutputs);
        assertEquals(1, simc.messageStorage.size());
        assertTrue(simc.messageStorage.containsValue(mr));
        assertEquals(1, simc.waveSimInputs.size());
        assertEquals(1, simc.waveSimInputs.get(0).outgoingMessages.size());
        assertEquals(start + 1, simc.messId);
        assertEquals("" + start, simc.waveSimInputs.get(0).outgoingMessages.get(0).messageId);
        assertEquals(testMac, simc.waveSimInputs.get(0).outgoingMessages.get(0).destination);
    }

    @Test
    public void testGetWaveSimInputs() {
        simc.waveSimInputs = null;
        Iterable<Tx> actual = simc.getWaveSimInputs(0, null);
        assertNull(actual);
    }

    @Test
    public void testGetIndicationsForDevicesByTimeStep1() {
        simc.appInputs = testAppInputs;
        assertEquals(testAppInputStep, simc.getIndicationsForDevicesByTimeStep(goodInsertId));
    }

    @Test
    public void testGetIndicationsForDevicesByTimeStep2() {
        simc.appInputs = testAppInputs;
        assertEquals(new HashMap<Long, List<BasicMessage>>(), simc.getIndicationsForDevicesByTimeStep(goodInsertId + 1));
    }

    @Test
    public void testGetCurrentMessages1() {
        assertTrue(simc.getTxMessages(0).isEmpty());
    }

    @Test
    public void testGetCurrentMessages2() {
        simc.messageStorage.put(mrConstStr, mr);
        Iterable<List<BasicMessage>> messageListIter = simc.getTxMessages(0).values();

        boolean looped = false;
        for (List<BasicMessage> messageList : messageListIter) {

            for (BasicMessage message : messageList) {
                assertTrue(((String)message.getData()).equals(mr.getData()));
                looped = true;
            }
        }
        assertTrue(looped);
    }

    /**
     * Tests the <code>putWaveSimOutputs</code> method in the default message component.
     */
    @Test
    public void testPutWaveSimOutputs() {

        // add application layer outputs
        BasicMessage message = new DSRCMessage(new Object(), DSRCChannel.CH184, 203040, 0, 102030);
        AppLayerOutput output = new AppLayerOutput("Application Alpha", 102030, new DistanceImpl(0.0, 0.0, 0.0));
        output.addMessagesList(Arrays.asList(message));
        output.setMessageType(MessageType.DSRC);

        BasicMessage message2 = new DSRCMessage(new Object(), DSRCChannel.CH184, 102030, 0, 203040);
        AppLayerOutput output2 = new AppLayerOutput("Application Beta", 203040, new DistanceImpl(50.0, 50.0, 50.0));
        output2.addMessagesList(Arrays.asList(message2));
        output2.setMessageType(MessageType.DSRC);

        BasicMessage message3 = new DSRCMessage(new Object(), DSRCChannel.CH184, 102030, 0, 304050);
        AppLayerOutput output3 = new AppLayerOutput("Application Delta", 304050, new DistanceImpl(100.0, 100.0, 100.0));
        output3.addMessagesList(Arrays.asList(message3));
        output3.setMessageType(MessageType.DSRC);

        simc.putAppLayerOutputs(0, Arrays.asList(output, output2, output3));

        // add WAVE simulation outputs
        Rx input = new Rx(102030);
        input.messages.put(Long.toString(1L), 1);
        Rx input2 = new Rx(203040);
        input2.messages.put(Long.toString(0L), 1);
        Rx input3 = new Rx(102030);
        input3.messages.put(Long.toString(2L), 1);

        simc.putWaveSimOutputs(0, Arrays.asList(input, input2, input3));

        // confirm that the received messages were added
        Map<Long, List<BasicMessage>> receipts = simc.getIndicationsForDevicesByTimeStep(1);

        int messageCount = 0;

        for (List<BasicMessage> messages : receipts.values()) {

            messageCount += messages.size();
        }

        assertTrue(messageCount == 3);
    }
}
