/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.datalayer.inmemory;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.etexascode.appslayerdata.AppLayerOutput;
import org.etexascode.datalayer.interfaces.IMessageComponent;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.wavesim.Rx;
import org.etexascode.wavesim.Tx;
import org.etexascode.wavesim.Tx.MessageType;
import org.junit.Before;
import org.junit.Test;

/**
 * Unit tests for the default message component.
 * 
 * @author emyers
 */
public class DefaultMessageComponentTest {

    /** The message component. */
    private IMessageComponent messageComponent;

    /**
     * Creates a new default message component before each test.
     */
    @Before
    public void init() {

        messageComponent = new DefaultMessageComponent();
    }

    /**
     * Tests the <code>putAppLayerOutputs</code> method in the default message component.
     */
    @Test
    public void testPutAppLayerOutputs() {

        // add application layer outputs
        BasicMessage message = new DSRCMessage(new Object(), DSRCChannel.CH184, 203040);
        AppLayerOutput output = new AppLayerOutput("Application Alpha", 102030, new DistanceImpl(0.0, 0.0, 0.0));
        output.addMessagesList(Arrays.asList(message));
        output.setMessageType(MessageType.DSRC);

        BasicMessage message2 = new DSRCMessage(new Object(), DSRCChannel.CH184, 102030);
        AppLayerOutput output2 = new AppLayerOutput("Application Beta", 203040, new DistanceImpl(50.0, 50.0, 50.0));
        output2.addMessagesList(Arrays.asList(message2));

        messageComponent.putAppLayerOutputs(0, Arrays.asList(output, output2));

        boolean containsGoodMessage = false;
        // confirm that the message with a valid message type was added
        for (List<BasicMessage> messageList : messageComponent.getTxMessages(0).values()) {

            if (messageList.contains(message)) {

                containsGoodMessage = true;
                break;
            }
        }
        assertTrue(containsGoodMessage);

        boolean containsBadMessage = false;
        // confirm that the message with a valid message type was added
        for (List<BasicMessage> messageList : messageComponent.getTxMessages(0).values()) {

            if (messageList.contains(message2)) {

                containsBadMessage = true;
                break;
            }
        }
        assertFalse(containsBadMessage);

        // confirm that the appropriate number of transmissions were created
        int txCount = 0;
        Iterator<Tx> transmissions = messageComponent.getWaveSimInputs(0, null).iterator();

        while (transmissions.hasNext()) {

            txCount++;
            transmissions.next();
        }

        assertTrue(txCount == 1);
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

        messageComponent.putAppLayerOutputs(0, Arrays.asList(output, output2, output3));

        // add WAVE simulation outputs
        Rx input = new Rx(102030);
        input.messages.put(Long.toString(1L), 1);
        Rx input2 = new Rx(203040);
        input2.messages.put(Long.toString(0L), 1);
        Rx input3 = new Rx(102030);
        input3.messages.put(Long.toString(2L), 1);

        messageComponent.putWaveSimOutputs(0, Arrays.asList(input, input2, input3));

        // confirm that the received messages were added
        Map<Long, List<BasicMessage>> receipts = messageComponent.getIndicationsForDevicesByTimeStep(1);

        int messageCount = 0;

        for (List<BasicMessage> messages : receipts.values()) {

            messageCount += messages.size();
        }

        assertTrue(messageCount == 3);
    }
}
