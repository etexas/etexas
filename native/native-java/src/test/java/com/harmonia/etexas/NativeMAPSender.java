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
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneMovement;
import org.etexascode.interrep.datamodel.LaneMovement.Movement;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.wavesim.WaveMessage;

import com.harmonia.apps.MapDataProducerApp;
import com.harmonia.etexas.wave.NativeAppManager;

/**
 * Sends some MapData to the native-agent running at the host name and port number given on the
 * command line.
 *
 * @author ttevendale
 */
public class NativeMAPSender {

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
        NativeApp<OBUDeviceInfo> nativeAppInstance2 = new NativeApp<OBUDeviceInfo>("app2", "native-base-app.uexe", hostName, portNumber);
        NativeApp<OBUDeviceInfo> nativeAppInstance3 = new NativeApp<OBUDeviceInfo>("app3", "native-base-app.uexe", hostName, portNumber);
        appList.add(nativeAppInstance1);
        appList.add(nativeAppInstance2);
        appList.add(nativeAppInstance3);

        MapDataProducerApp mp = new MapDataProducerApp();
        LaneManager laneMan = new LaneManager();
        Lane lane1 = new Lane();
        Lane lane2 = new Lane();
        Lane lane3 = new Lane();

        laneMan.setLatitude(10);
        laneMan.setLongitude(5);
        laneMan.setElevation(15);
        laneMan.setIntersectionId(1);

        /** Set up lanes */

        Map<Integer, LaneMovement> laneMovements = new HashMap<Integer, LaneMovement>();
        Map<Integer, LaneMovement> laneMovements2 = new HashMap<Integer, LaneMovement>();
        Map<Integer, LaneMovement> laneMovements3 = new HashMap<Integer, LaneMovement>();

        LaneMovement lm = new LaneMovement();
        LaneMovement lm2 = new LaneMovement();
        LaneMovement lm3 = new LaneMovement();
        LaneMovement lm4 = new LaneMovement();
        lm.setMovementId(1);
        lm.setMovement(Movement.LEFT_TURN);
        lm2.setMovementId(2);
        lm2.setMovement(Movement.LEFT_TURN_ON_RED);
        lm3.setMovementId(3);
        lm3.setMovement(Movement.STRAIGHT);
        lm4.setMovementId(4);
        lm4.setMovement(Movement.RIGHT_TURN_ON_RED);

        laneMovements.put(1, lm);
        laneMovements2.put(1, lm3);
        laneMovements2.put(2, lm4);

        LaneNode node1 = new LaneNode(-146, 8432);
        LaneNode node2 = new LaneNode(-200, 9225);
        LaneNode node3 = new LaneNode(-466, 9000);
        LaneNode node4 = new LaneNode(-500, 10000);
        LaneNode node5 = new LaneNode(155, 8679);
        LaneNode node6 = new LaneNode(100, 9288);

        lane1.setApproachId(4);
        lane1.setLaneId(1);
        lane1.setType(Lane.INBOUND);
        lane1.setLaneMovements(laneMovements);
        lane1.addLaneNode(node1);
        lane1.addLaneNode(node2);

        lane2.setApproachId(4);
        lane2.setLaneId(2);
        lane2.setLaneMovements(laneMovements2);
        lane2.addLaneNode(node3);
        lane2.addLaneNode(node4);
        lane2.setType(Lane.INBOUND);

        lane3.setApproachId(5);
        lane3.setLaneId(3);
        lane3.addLaneNode(node5);
        lane3.addLaneNode(node6);
        ;
        lane3.setType(Lane.OUTBOUND);

        Map<Integer, Lane> lanes = new HashMap<Integer, Lane>();

        lanes.put(1, lane1);
        lanes.put(2, lane2);
        lanes.put(3, lane3);

        laneMan.setLanes(lanes);

        byte[] bytes = mp.buildRawMapData(laneMan);
        System.out.println("\nMapData: size: " + bytes.length);
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
        nativeAppManager.sendInputs(inputs, 12.0);
        Thread.sleep(5000);
        nativeAppManager.sendInputs(inputs, 42.0);
        Thread.sleep(3000);
        // List<AppLayerOutput> appsOutput = nativeAppManager.getOutputs();
        // AppLayerOutput appOutput = appsOutput.get(0);
        // System.out.println("printing appId: " + appOutput.appId);
        nativeAppManager.deregisterExecution();
    }
}
