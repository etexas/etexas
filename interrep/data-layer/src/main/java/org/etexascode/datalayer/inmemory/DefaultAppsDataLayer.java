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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.etexascode.appslayerdata.AppLayerInput;
import org.etexascode.appslayerdata.AppLayerOutput;
import org.etexascode.appslayerdata.CellularDeviceInfo;
import org.etexascode.appslayerdata.IDeviceInfo;
import org.etexascode.appslayerdata.OBUDeviceInfo;
import org.etexascode.appslayerdata.RSEDeviceInfo;
import org.etexascode.appslayerdata.ReportDeviceInfo;
import org.etexascode.datalayer.interfaces.IAppsDataLayer;
import org.etexascode.datalayer.interfaces.ICommandsComponent;
import org.etexascode.datalayer.interfaces.IDevicesComponent;
import org.etexascode.datalayer.interfaces.IIntersectionModelsComponent;
import org.etexascode.datalayer.interfaces.IMacManagerComponent;
import org.etexascode.datalayer.interfaces.IMessageComponent;
import org.etexascode.datalayer.interfaces.ITemporalComponent;
import org.etexascode.devicedata.AbstractEmbeddedDeviceData;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.CellDeviceData;
import org.etexascode.devicedata.FixedCellDeviceData;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.devicedata.LogData;
import org.etexascode.devicedata.OBUDeviceData;
import org.etexascode.devicedata.RSEDeviceData;
import org.etexascode.devicedata.ReportDeviceData;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.persistencelayer.IPersistenceLayer;

/**
 * The default application data layer.
 * 
 * @author ablatt
 * @author ttevendale
 * @author emyers
 */
public class DefaultAppsDataLayer implements IAppsDataLayer {

    /** The commands component. */
    private ICommandsComponent commandsComponent;

    /** The devices component. */
    private IDevicesComponent devicesComponent;

    /** The intersecion models component. */
    private IIntersectionModelsComponent modelsComponent;

    /** The MAC manager component. */
    private IMacManagerComponent macManagerComponent;

    /** The message component. */
    private IMessageComponent messageComponent;

    /** The persistence layer. */
    private IPersistenceLayer persistenceLayer;

    /** The temporal component. */
    private ITemporalComponent temporalComponent;

    /** The report device location. */
    private static final DistanceImpl REPORT_DEVICE_LOCATION = new DistanceImpl(0, 0, 0);

    /**
     * Creates a new <code>DefaultAppsDataLayer</code> with the specified components.
     * 
     * @param commandsComponent The commands component for this data layer.
     * @param devicesComponent The devices component for this data layer.
     * @param modelsComponent The models component for this data layer.
     * @param macManagerComponent The MAC manager component for this data layer.
     * @param messageComponent The message component for this data layer.
     * @param persistenceLayer The persistence layer for this data layer.
     * @param temporalComponent The temporal component for this data layer.
     */
    public DefaultAppsDataLayer(ICommandsComponent commandsComponent, IDevicesComponent devicesComponent, IIntersectionModelsComponent modelsComponent, IMacManagerComponent macManagerComponent,
            IMessageComponent messageComponent, IPersistenceLayer persistenceLayer, ITemporalComponent temporalComponent) {

        this.commandsComponent = commandsComponent;
        this.devicesComponent = devicesComponent;
        this.modelsComponent = modelsComponent;
        this.macManagerComponent = macManagerComponent;
        this.messageComponent = messageComponent;
        this.persistenceLayer = persistenceLayer;
        this.temporalComponent = temporalComponent;
    }

    @Override
    public List<AppLayerInput> getAppData(int stepNum) {

        Map<Long, List<BasicMessage>> stepMesses = messageComponent.getIndicationsForDevicesByTimeStep(stepNum);

        // collect all messages received in this step to pass to the report device
        List<BasicMessage> rxMessages = new LinkedList<BasicMessage>();

        for (List<BasicMessage> messageList : stepMesses.values()) {

            if (messageList != null) {

                rxMessages.addAll(messageList);
            }
        }

        List<AppLayerInput> ret = new LinkedList<AppLayerInput>();

        // now iterate over all active devices and perform app input logic
        for (IDeviceData idd : devicesComponent.getActiveDevices(stepNum)) {

            IDeviceInfo devInf = null;
            long mac = macManagerComponent.getMac(stepNum, idd.getProperId());
            idd.setMacAddress(mac);

            if (idd instanceof ReportDeviceData) {

                Map<Integer, IVehicleManager> vehicleMap = new HashMap<Integer, IVehicleManager>();
                Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>();
                Map<Integer, ILaneManager> laneMap = new HashMap<Integer, ILaneManager>();
                Map<Integer, IDetectorManager> detectorMap = new HashMap<Integer, IDetectorManager>();

                for (int intersection : ((ReportDeviceData)idd).getIntersections()) {

                    InterRepInfoModel irim = modelsComponent.getInfoModel(stepNum, intersection);
                    vehicleMap.put(intersection, irim.vmi);
                    signalMap.put(intersection, irim.smi);
                    laneMap.put(intersection, irim.lmi);
                    detectorMap.put(intersection, irim.dmi);
                }

                Collection<BasicMessage> wsmIndications = new ArrayList<BasicMessage>();
                for (List<BasicMessage> messages : messageComponent.getTxMessages(stepNum).values()) {
                    wsmIndications.addAll(messages);
                }

                devInf = new ReportDeviceInfo(vehicleMap, signalMap, laneMap, detectorMap, wsmIndications, rxMessages, mac, REPORT_DEVICE_LOCATION);
            }
            else {

                List<BasicMessage> inds = stepMesses.get(mac);

                if (inds == null) {

                    inds = new ArrayList<BasicMessage>(0);
                }

                if (idd instanceof OBUDeviceData) {

                    devInf = new OBUDeviceInfo(modelsComponent.getVehicleInfoById(stepNum, ((AbstractEmbeddedDeviceData)idd).getVehicleId()), inds, mac);
                }
                else if (idd instanceof CellDeviceData) {

                    IDistanceable location = modelsComponent.getVehicleInfoById(stepNum, ((AbstractEmbeddedDeviceData)idd).getVehicleId());
                    devInf = new CellularDeviceInfo(location, inds, mac);
                }
                else if (idd instanceof RSEDeviceData) {

                    Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>();
                    Map<Integer, ILaneManager> laneMap = new HashMap<Integer, ILaneManager>();
                    Map<Integer, IDetectorManager> detectorMap = new HashMap<Integer, IDetectorManager>();

                    for (int intersection : ((RSEDeviceData)idd).getIntersections()) {

                        InterRepInfoModel irim = modelsComponent.getInfoModel(stepNum, intersection);
                        signalMap.put(intersection, irim.smi);
                        laneMap.put(intersection, irim.lmi);
                        detectorMap.put(intersection, irim.dmi);
                    }

                    devInf = new RSEDeviceInfo(signalMap, laneMap, detectorMap, inds, ((RSEDeviceData)idd).getReferencePoints(), mac, (RSEDeviceData)idd);
                }
                else if (idd instanceof FixedCellDeviceData) {

                    FixedCellDeviceData fcdd = (FixedCellDeviceData)idd;
                    IDistanceable location = new DistanceImpl(fcdd.x, fcdd.y, fcdd.z);
                    devInf = new CellularDeviceInfo(location, inds, mac);
                }
                else {

                    throw new AssertionError("Device Data used that is not supported.");
                }
            }

            for (IConnectedVehicleApp<?> app : idd.getApps()) {

                ret.add(new AppLayerInput(app, devInf));
            }
        }

        return ret;
    }

    @Override
    public void putAppOutputs(int stepNum, List<AppLayerOutput> appOutputs) {

        Iterable<IDeviceData> devices = devicesComponent.getActiveDevices(stepNum);
        Map<Long, IDistanceable> locationsMap = new HashMap<Long, IDistanceable>();
        for (IDeviceData device : devices) {

            if (device instanceof AbstractEmbeddedDeviceData) {

                locationsMap.put(device.getMacAddress(), modelsComponent.getVehicleInfoById(stepNum, ((AbstractEmbeddedDeviceData)device).getVehicleId()));
            }
            else if (device instanceof IDistanceable) {

                locationsMap.put(device.getMacAddress(), (IDistanceable)device);
            }
            else {

                continue;
            }
        }
        messageComponent.putAllDeviceOutputs(stepNum, appOutputs, locationsMap, devices);

        /*
         * Note: ablatt - is this faster than just adding the logs straight to the persistence layer
         * in practice?
         */

        List<LogData> logs = new LinkedList<LogData>();

        for (AppLayerOutput alo : appOutputs) {

            // Note: ablatt - this means that this cannot be used for multiple intersections...

            commandsComponent.putSignalCommands(stepNum, 0, alo.getSigCommands());
            commandsComponent.putVehicleCommands(stepNum, 0, alo.getVehCommands());
            persistenceLayer.persistSignalCommands(alo.appName, 0, temporalComponent.getSimTime(stepNum), alo.getSigCommands());
            persistenceLayer.persistVehicleCommands(alo.appName, 0, temporalComponent.getSimTime(stepNum), alo.getVehCommands());
            logs.addAll(alo.getLogs());
        }

        persistenceLayer.persistAppLogs(logs);
    }

    @Override
    public double getSimTime(int stepNum) {

        return temporalComponent.getSimTime(stepNum);
    }
}
