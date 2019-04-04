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

import java.awt.Polygon;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.appslayerdata.AppLayerOutput;
import org.etexascode.datalayer.interfaces.IMessageComponent;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.CellDeviceData;
import org.etexascode.devicedata.FixedCellDeviceData;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.devicedata.OBUDeviceData;
import org.etexascode.devicedata.RSEDeviceData;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.wavesim.Rx;
import org.etexascode.wavesim.Tx;
import org.etexascode.wavesim.Tx.MessageType;
import org.etexascode.wavesim.WaveMessage;

/**
 * The default message component.
 * 
 * @author ablatt
 * @author emyers
 * @author ttevendale
 */
public class DefaultMessageComponent implements IMessageComponent {

    /** The list of WAVE simulation inputs. */
    private List<Tx> waveSimInputs;

    /** The message ID accumulator. */
    private long messageId;

    /** The map of application input mappings. */
    private Map<Integer, Map<Long, List<BasicMessage>>> appInputsMap;

    /** the map of messages that were recently transmitted/about to be */
    private Map<Long, List<BasicMessage>> txMessageMap;

    /** The map of stored messages. */
    private Map<String, BasicMessage> messageStorageMap;

    /**
     * Creates a new <code>DefaultMessageComponent</code> instance.
     */
    public DefaultMessageComponent() {

        messageId = 0;
        waveSimInputs = new ArrayList<Tx>();
        messageStorageMap = new HashMap<String, BasicMessage>();
        txMessageMap = new HashMap<Long, List<BasicMessage>>();
        appInputsMap = new HashMap<Integer, Map<Long, List<BasicMessage>>>();
    }

    @Override
    public void putAppLayerOutputs(int stepNum, Collection<AppLayerOutput> outputs) {

        messageStorageMap.clear();
        txMessageMap.clear();
        appInputsMap.remove(stepNum - 1);
        waveSimInputs = new ArrayList<Tx>(outputs.size());

        for (AppLayerOutput output : outputs) {

            if (output.getMessageType() != null) {

                List<WaveMessage> messages = new ArrayList<WaveMessage>(output.getOutMessages().size());

                for (BasicMessage message : output.getOutMessages()) {

                    String id = String.valueOf(messageId++);
                    messageStorageMap.put(id, message);
                    messages.add(new WaveMessage(id, message.getPeerMACAddress(), message.getSize()));
                }

                txMessageMap.put(output.mac, output.getOutMessages());
                waveSimInputs.add(new Tx(output.getMessageType(), output.location, output.mac, messages));
            }
        }
    }

    @Override
    public void putAllDeviceOutputs(int stepNum, Collection<AppLayerOutput> outputs, Map<Long, IDistanceable> locationsMap, Iterable<IDeviceData> devices) {

        putAppLayerOutputs(stepNum, outputs);
        for (IDeviceData device : devices) {

            if (!txMessageMap.containsKey(device.getMacAddress())) {

                MessageType messageType;
                if (device instanceof OBUDeviceData || device instanceof RSEDeviceData) {

                    messageType = MessageType.DSRC;
                }
                else if (device instanceof CellDeviceData || device instanceof FixedCellDeviceData) {

                    messageType = MessageType.CELLULAR;
                }
                else {

                    continue;
                }
                txMessageMap.put(device.getMacAddress(), new ArrayList<BasicMessage>(0));
                waveSimInputs.add(new Tx(messageType, locationsMap.get(device.getMacAddress()), device.getMacAddress(), new ArrayList<WaveMessage>(0)));
            }
        }
    }

    @Override
    public void putWaveSimOutputs(int stepNum, Iterable<Rx> outputs) {

        for (Rx received : outputs) {

            for (Entry<String, Integer> entry : received.messages.entrySet()) {

                // Create a new message for the Rx so that separate objects exist for Rxs vs Txs.
                BasicMessage message = messageStorageMap.get(entry.getKey());
                BasicMessage routedInd = message.copy();
                routedInd.setPeerMACAddress(received.mac);

                // Get the time step that Rx will occur.
                int timeStep = entry.getValue();

                // Get map of messages keyed by device ID.
                Map<Long, List<BasicMessage>> messageMap = appInputsMap.get(timeStep);

                if (messageMap == null) {

                    messageMap = new HashMap<Long, List<BasicMessage>>();
                    appInputsMap.put(timeStep, messageMap);
                }

                // Get list of messages for device id.
                List<BasicMessage> deviceMessages = messageMap.get(received.mac);

                if (deviceMessages == null) {

                    deviceMessages = new LinkedList<BasicMessage>();
                    messageMap.put(received.mac, deviceMessages);
                }

                // Add new message for device.
                deviceMessages.add(routedInd);
            }
        }
    }

    @Override
    public Iterable<Tx> getWaveSimInputs(int stepNum, Polygon p) {

        return waveSimInputs;
    }

    @Override
    public Map<Long, List<BasicMessage>> getIndicationsForDevicesByTimeStep(int stepNum) {

        Map<Long, List<BasicMessage>> inputMap = appInputsMap.get(stepNum);
        return (inputMap != null) ? Collections.unmodifiableMap(inputMap) : new HashMap<Long, List<BasicMessage>>();
    }

    @Override
    public Map<Long, List<BasicMessage>> getTxMessages(int stepNum) {

        return Collections.unmodifiableMap(txMessageMap);
    }
}
