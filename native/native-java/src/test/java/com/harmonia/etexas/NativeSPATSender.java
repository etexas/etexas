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

package com.harmonia.etexas;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.etexascode.appslayerdata.AppLayerInput;
import org.etexascode.appslayerdata.OBUDeviceInfo;
import org.etexascode.appslayerdata.RSEDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.NativeApp;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.wavesim.WaveMessage;

import com.harmonia.apps.SPATProducerApp;
import com.harmonia.etexas.wave.NativeAppManager;

/**
 * Sends some SPAT data to the native-agent running at the host name and port number given on the
 * command line.
 *
 * @author ttevendale
 */
public class NativeSPATSender {

    /**
     * Main method
     * 
     * @param args the command line arguments
     * @throws java.lang.InterruptedException
     */
    public static void main(String[] args) throws InterruptedException {
        if (args.length != 2) {
            System.err.println("Usage: command <host name> <port number>");
            System.exit(1);
        }

        String hostName = args[0];
        int portNumber = Integer.parseInt(args[1]);

        LinkedList<NativeApp<?>> appList = new LinkedList<NativeApp<?>>();
        NativeApp<OBUDeviceInfo> nativeAppInstance1 = new NativeApp<OBUDeviceInfo>("app1", "native-base-app.uexe", hostName, portNumber);
        appList.add(nativeAppInstance1);

        SPATProducerApp sp = new SPATProducerApp();
        SignalManager sm = new SignalManager();
        SignalIndication si1 = new SignalIndication();
        SignalIndication si2 = new SignalIndication();
        SignalIndication si3 = new SignalIndication();

        si1.setColorIndication(SignalIndication.Color.RED);
        si1.setLaneId(1);
        si1.setStateIndication(SignalIndication.State.STEADY);
        si1.setTimeToChange(5);
        si1.setTypeIndication(SignalIndication.Type.BALL);

        si2.setColorIndication(SignalIndication.Color.GREEN);
        si2.setLaneId(1);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTimeToChange(10);
        si2.setTypeIndication(SignalIndication.Type.BALL);

        si3.setColorIndication(SignalIndication.Color.YELLOW);
        si3.setLaneId(2);
        si3.setStateIndication(SignalIndication.State.STEADY);
        si3.setTimeToChange(5);
        si3.setTypeIndication(SignalIndication.Type.BALL);

        sm.addSignal(si1);
        sm.addSignal(si2);
        sm.addSignal(si3);

        byte[] bytes = sp.buildSPAT(0, sm);
        System.out.println("\nSPAT data: size: " + bytes.length);
        for (int i = 0; i < bytes.length; i++) {
            System.out.print(String.format("%02X", bytes[i]));
        }
        System.out.println();

        List<BasicMessage> messages = new ArrayList<BasicMessage>();

        messages.add(new DSRCMessage(bytes, DSRCChannel.CH184, WaveMessage.MACBROADCAST, bytes.length));

        Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>();
        Map<Integer, ILaneManager> laneMap = new HashMap<Integer, ILaneManager>();
        Map<Integer, IDetectorManager> detectorMap = new HashMap<Integer, IDetectorManager>();
        signalMap.put(0, new SignalManager());
        laneMap.put(0, new LaneManager());
        detectorMap.put(0, new DetectorManager());
        RSEDeviceInfo dev = new RSEDeviceInfo(signalMap, laneMap, detectorMap, messages, new ReferencePoint[0], 42, new DistanceImpl(0.1, 0.2, 0.3));

        NativeAppManager nativeAppManager = new NativeAppManager(8L);
        nativeAppManager.registerNativeApps(appList);
        Thread.sleep(5000);
        List<AppLayerInput> inputs = new ArrayList<AppLayerInput>(1);
        AppLayerInput input = new AppLayerInput(nativeAppInstance1, dev);
        inputs.add(input);
        nativeAppManager.sendInputs(inputs, 0.0);
        Thread.sleep(5000);
        nativeAppManager.sendInputs(inputs, 1.0);
        // List<AppLayerOutput> appsOutput = nativeAppManager.getOutputs();
        // AppLayerOutput appOutput = appsOutput.get(0);
        // System.out.println("printing appId: " + appOutput.appId);
        nativeAppManager.deregisterExecution();
    }
}
