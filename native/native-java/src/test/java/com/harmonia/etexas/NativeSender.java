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
import org.etexascode.appslayerdata.AppLayerOutput;
import org.etexascode.appslayerdata.OBUDeviceInfo;
import org.etexascode.appslayerdata.RSEDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.NativeApp;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;

import com.harmonia.etexas.wave.NativeAppManager;

/**
 * Sends some test data to the native-agent running at the host name and port number given on the
 * command line.
 *
 * @author janway
 */
public class NativeSender {

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

        Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>();
        Map<Integer, ILaneManager> laneMap = new HashMap<Integer, ILaneManager>();
        Map<Integer, IDetectorManager> detectorMap = new HashMap<Integer, IDetectorManager>();
        signalMap.put(0, new SignalManager());
        laneMap.put(0, new LaneManager());
        detectorMap.put(0, new DetectorManager());
        RSEDeviceInfo dev = new RSEDeviceInfo(signalMap, laneMap, detectorMap, new ArrayList<BasicMessage>(0), new ReferencePoint[0], 42, new DistanceImpl(0.1, 0.2, 0.3));

        NativeAppManager nativeAppManager = new NativeAppManager(8L);
        nativeAppManager.registerNativeApps(appList);
        Thread.sleep(5000);
        List<AppLayerInput> inputs = new ArrayList<AppLayerInput>(1);
        AppLayerInput input = new AppLayerInput(nativeAppInstance1, dev);
        inputs.add(input);
        nativeAppManager.sendInputs(inputs, 0.0);
        Thread.sleep(5000);
        nativeAppManager.sendInputs(inputs, 1.0);
        List<AppLayerOutput> appsOutput = nativeAppManager.getOutputs();
        AppLayerOutput appOutput = appsOutput.get(0);
        System.out.println("printing appId: " + appOutput.appName);
        nativeAppManager.deregisterExecution();
    }
}
