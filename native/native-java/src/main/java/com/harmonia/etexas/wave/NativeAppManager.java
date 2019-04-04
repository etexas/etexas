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

package com.harmonia.etexas.wave;

import java.awt.Polygon;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.lang3.SerializationUtils;
import org.etexascode.appslayer.INativeAppManager;
import org.etexascode.appslayerdata.AppLayerInput;
import org.etexascode.appslayerdata.AppLayerOutput;
import org.etexascode.appslayerdata.IDeviceInfo;
import org.etexascode.appslayerdata.OBUDeviceInfo;
import org.etexascode.appslayerdata.RSEDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.LogData;
import org.etexascode.devicedata.NativeApp;
import org.etexascode.driver.SimDriverException;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleDestinationCommand;
import org.etexascode.interrep.datamodel.VehicleLaneChangeCommand;
import org.etexascode.interrep.datamodel.VehicleSpeedCommand;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.IDetectorEvent;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneMovement;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;

import com.google.protobuf.ByteString;
import com.harmonia.etexas.wave.NativeAppProtobuf.AppDevMap;
import com.harmonia.etexas.wave.NativeAppProtobuf.AppInitConfig;
import com.harmonia.etexas.wave.NativeAppProtobuf.BundleWrapper;
import com.harmonia.etexas.wave.NativeAppProtobuf.DetectorManagerData;
import com.harmonia.etexas.wave.NativeAppProtobuf.DetectorManagerData.DetectorData.DetectorEventData;
import com.harmonia.etexas.wave.NativeAppProtobuf.DetectorManagerData.DetectorData.PolygonData;
import com.harmonia.etexas.wave.NativeAppProtobuf.DetectorManagerEntry;
import com.harmonia.etexas.wave.NativeAppProtobuf.InitBundle;
import com.harmonia.etexas.wave.NativeAppProtobuf.InputBundle;
import com.harmonia.etexas.wave.NativeAppProtobuf.LaneManagerData;
import com.harmonia.etexas.wave.NativeAppProtobuf.LaneManagerData.LaneData;
import com.harmonia.etexas.wave.NativeAppProtobuf.LaneManagerData.LaneData.LaneMovement;
import com.harmonia.etexas.wave.NativeAppProtobuf.LaneManagerData.LaneData.LaneMovement.Movement;
import com.harmonia.etexas.wave.NativeAppProtobuf.LaneManagerData.LaneData.LaneNode;
import com.harmonia.etexas.wave.NativeAppProtobuf.LaneManagerEntry;
import com.harmonia.etexas.wave.NativeAppProtobuf.ObuDevice;
import com.harmonia.etexas.wave.NativeAppProtobuf.OutputBundle;
import com.harmonia.etexas.wave.NativeAppProtobuf.RseDevice;
import com.harmonia.etexas.wave.NativeAppProtobuf.RseModelData;
import com.harmonia.etexas.wave.NativeAppProtobuf.ShutdownBundle;
import com.harmonia.etexas.wave.NativeAppProtobuf.SignalCommandData;
import com.harmonia.etexas.wave.NativeAppProtobuf.SignalManagerData;
import com.harmonia.etexas.wave.NativeAppProtobuf.SignalManagerEntry;
import com.harmonia.etexas.wave.NativeAppProtobuf.Tx;
import com.harmonia.etexas.wave.NativeAppProtobuf.VehicleCommandData;
import com.harmonia.etexas.wave.NativeAppProtobuf.VehicleData;
import com.harmonia.etexas.wave.NativeAppProtobuf.VehicleData.VehicleType;

import nanomsg.exceptions.IOException;
import nanomsg.reqrep.ReqSocket;

public class NativeAppManager implements INativeAppManager {

    /**
     * The id of the execution this manager belongs to.
     */
    private final Long execId;

    /**
     * Counter used for uniquely identifying messages.
     */
    private Long currMessId = 0L;

    /**
     * The native apps, grouped by the VM they're hosted on. Outer key is hostName, inner key is
     * port number, value is apps at that host:port.
     */
    private Map<String, Map<Integer, List<NativeApp<?>>>> appsByVM = new HashMap<String, Map<Integer, List<NativeApp<?>>>>();

    /**
     * The output from the apps
     */
    BundleWrapper output;

    /**
     * Constructor.
     * 
     * @param execId The execution id.
     */
    public NativeAppManager(Long execId) {
        this.execId = execId;
    }

    @Override
    public void registerNativeApps(List<NativeApp<?>> apps) {
        // Group the apps by the VM they're running on
        Map<String, Map<Integer, List<NativeApp<?>>>> newAppsByVM = new HashMap<String, Map<Integer, List<NativeApp<?>>>>();
        for (NativeApp<?> app : apps) {
            // Update a local map of apps.
            Map<Integer, List<NativeApp<?>>> newHost = newAppsByVM.get(app.getHostName());
            if (newHost == null) {
                newHost = new HashMap<Integer, List<NativeApp<?>>>();
                newAppsByVM.put(app.getHostName(), newHost);
            }
            List<NativeApp<?>> newAppsList = newHost.get(app.getPortNumber());
            if (newAppsList == null) {
                newAppsList = new LinkedList<NativeApp<?>>();
                newHost.put(app.getPortNumber(), newAppsList);
            }
            newAppsList.add(app);

            // Update the global map.
            Map<Integer, List<NativeApp<?>>> host = appsByVM.get(app.getHostName());
            if (host == null) {
                host = new HashMap<Integer, List<NativeApp<?>>>();
                appsByVM.put(app.getHostName(), host);
            }
            List<NativeApp<?>> appsList = host.get(app.getPortNumber());
            if (appsList == null) {
                appsList = new LinkedList<NativeApp<?>>();
                host.put(app.getPortNumber(), appsList);
            }
            appsList.add(app);
        }

        // Create and send InitBundles to each VM
        for (Entry<String, Map<Integer, List<NativeApp<?>>>> hostMap : newAppsByVM.entrySet()) {
            Map<Integer, List<NativeApp<?>>> innerMap = hostMap.getValue();
            for (Entry<Integer, List<NativeApp<?>>> portMap : innerMap.entrySet()) {
                List<NativeApp<?>> appsOnHostAndPortList = portMap.getValue();
                InitBundle initBundle = createInitBundle(appsOnHostAndPortList);
                BundleWrapper msg = BundleWrapper.newBuilder().setExecId(execId).setInitBundle(initBundle).build();
                try {
                    ReqSocket s = new ReqSocket();
                    s.connect("tcp://" + hostMap.getKey() + ":" + portMap.getKey());

                    // Send message that client is ready to start benchmarking
                    byte[] sendBytes = msg.toByteArray();
                    s.sendBytes(sendBytes);
                    s.recvString();
                    s.close();
                }
                catch (IOException ex) {
                    Logger.getLogger(NativeAppManager.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }
    }

    @Override
    public void deregisterExecution() {
        // Create and send ShutdownBundles to each VM
        for (Entry<String, Map<Integer, List<NativeApp<?>>>> hostEntry : appsByVM.entrySet()) {
            for (Integer port : hostEntry.getValue().keySet()) {
                ShutdownBundle shutdownBundle = createShutdownBundle();
                BundleWrapper msg = BundleWrapper.newBuilder().setExecId(execId).setShutdownBundle(shutdownBundle).build();
                try {
                    ReqSocket s = new ReqSocket();
                    s.connect("tcp://" + hostEntry.getKey() + ":" + port);

                    // Send message that client is ready to start benchmarking
                    byte[] sendBytes = msg.toByteArray();
                    s.sendBytes(sendBytes);
                    s.recvString();
                    s.close();
                }
                catch (IOException ex) {
                    Logger.getLogger(NativeAppManager.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }
    }

    /**
     * Helper method for getting the Vehicle commands out of the buffer
     * 
     * @param outputData the buffer which contains the apps output
     * @return vehCommands the list of vehcile commands
     */
    private List<VehicleCommand> retrieveAllVehicleCommands(OutputBundle outputData) {
        List<VehicleCommand> vehCommands = new ArrayList<VehicleCommand>();
        for (int i = 0; i < outputData.getVehComsCount(); i++) {

            VehicleCommandData vehicleCommandData = outputData.getVehComs(i);
            VehicleCommand vehCommand;
            int command = vehicleCommandData.getCommand();
            int vehicleId = vehicleCommandData.getVehicleId();
            VehicleCommandData.VehicleCommandClass commandClass = vehicleCommandData.getCommandClass();

            if (commandClass == VehicleCommandData.VehicleCommandClass.DESTINATION) {
                vehCommand = new VehicleDestinationCommand(vehicleId, command);

            }
            else if (commandClass == VehicleCommandData.VehicleCommandClass.LANE_CHANGE) {
                vehCommand = new VehicleLaneChangeCommand(vehicleId, command);

            }
            else if (commandClass == VehicleCommandData.VehicleCommandClass.SPEED) {
                vehCommand = new VehicleSpeedCommand(vehicleId, command, vehicleCommandData.getSpeed());

            }
            else {
                vehCommand = null; // This might need to throw a WebAppException?
            }
            vehCommands.add(vehCommand);
        }
        return vehCommands;
    }

    /**
     * Helper method for getting the Signal commands out of the buffer
     * 
     * @param outputData the buffer which contains the apps output
     * @return sigCommands the list of signal commands
     */
    private List<SignalCommand> retrieveAllSignalCommands(OutputBundle outputData) {
        List<SignalCommand> sigCommands = new ArrayList<SignalCommand>();
        for (int i = 0; i < outputData.getSigComsCount(); i++) {
            SignalCommandData sigCommandData = outputData.getSigComs(i);
            SignalCommand sigCommand = new SignalCommand(sigCommandData.getCommand(), sigCommandData.getTime());
            sigCommands.add(sigCommand);
        }
        return sigCommands;
    }

    /**
     * Helper method for getting the messages out of the buffer
     * 
     * @param outputData the buffer which contains the apps output
     * @return outMessages the list of output messages
     */
    private List<BasicMessage> retrieveAllMessages(OutputBundle outputData) {
        List<BasicMessage> outMessages = new ArrayList<BasicMessage>();
        for (int i = 0; i < outputData.getMessagesCount(); i++) {
            com.harmonia.etexas.wave.NativeAppProtobuf.WSMIndication messageData = outputData.getMessages(i);
            BasicMessage message = new DSRCMessage(messageData.getData(), DSRCChannel.CH184, messageData.getDest(), messageData.getSize());
            message.setOriginMACAddress(messageData.getSrc());
            outMessages.add(message);
        }
        return outMessages;
    }

    /**
     * Helper method for getting the logs out of the buffer
     * 
     * @param outputData the buffer which contains the apps output
     * @return logs the list of logs
     */
    private List<LogData> retrieveAllLogs(OutputBundle outputData) {
        List<LogData> logs = new ArrayList<LogData>();
        long devId = outputData.getDevId();
        String appId = outputData.getAppId();

        for (int i = 0; i < outputData.getLogsCount(); i++) {
            com.harmonia.etexas.wave.NativeAppProtobuf.LogData logData = outputData.getLogs(i);
            LogData log = new LogData(devId, appId, new BigDecimal(logData.getSimTime()), logData.getKey(), logData.getMessage());
            logs.add(log);
        }
        return logs;
    }

    /**
     * This method gets the apps output and returns it as a list of AppLayerOutputs
     * 
     * @return appsOutput the list of AppLayerOutputs
     */
    @Override
    public List<AppLayerOutput> getOutputs() {
        if (output != null) {
            HashMap<String, AppLayerOutput> appsOutputHashMap = new HashMap<String, AppLayerOutput>();
            for (int i = 0; i < output.getOutputBundleCount(); i++) {

                OutputBundle outputData = output.getOutputBundle(i);
                String appId = outputData.getAppId();
                long devId = outputData.getDevId();

                // Checks to see if the AppLayerOutput exists and if not creates it
                if (appsOutputHashMap.get(appId + devId) == null) {
                    DistanceImpl location = new DistanceImpl(outputData.getX(), outputData.getY(), outputData.getZ());
                    appsOutputHashMap.put(appId + devId, new AppLayerOutput(appId, devId, location));
                }

                // gets the AppLayerOutput from the hashmap
                AppLayerOutput appOutput = appsOutputHashMap.get(appId + devId);

                // gets the rest of the data from this OutputBundle
                List<VehicleCommand> vehCommands = this.retrieveAllVehicleCommands(outputData);
                List<SignalCommand> sigCommands = this.retrieveAllSignalCommands(outputData);
                List<BasicMessage> outMessages = this.retrieveAllMessages(outputData);
                List<LogData> logs = this.retrieveAllLogs(outputData);

                // updates the AppLayerOutput
                appOutput.addVehicleCommandsList(vehCommands);
                appOutput.addSignalCommandsList(sigCommands);
                appOutput.addMessagesList(outMessages);
                appOutput.addLogsList(logs);

                // puts the updated AppLayerOutput back into the hashmap
                appsOutputHashMap.put(appId + devId, appOutput);
            }

            List<AppLayerOutput> appsOutput = new LinkedList<AppLayerOutput>();
            // Loops through the hashmap and makes a linked list out of Immutable AppLayerOutputs
            for (AppLayerOutput appOutput : appsOutputHashMap.values()) {
                appsOutput.add(appOutput.createImmutableCopy());
            }

            return appsOutput;
        }
        return new LinkedList<AppLayerOutput>();
    }

    @Override
    public void sendInputs(List<AppLayerInput> inputs, double simTime) {
        // Group the inputs by the VM they should be sent to
        Map<String, Map<Integer, List<AppLayerInput>>> inputsByVM = new HashMap<String, Map<Integer, List<AppLayerInput>>>();
        for (AppLayerInput ali : inputs) {
            NativeApp<?> app = (NativeApp<?>)ali.app;
            Map<Integer, List<AppLayerInput>> host = inputsByVM.get(app.getHostName());
            if (host == null) {
                host = new HashMap<Integer, List<AppLayerInput>>();
                inputsByVM.put(app.getHostName(), host);
            }
            List<AppLayerInput> appInputList = host.get(app.getPortNumber());
            if (appInputList == null) {
                appInputList = new LinkedList<AppLayerInput>();
                host.put(app.getPortNumber(), appInputList);
            }
            appInputList.add(ali);
        }

        // Create and send InputBundles to each VM
        for (Entry<String, Map<Integer, List<AppLayerInput>>> host : inputsByVM.entrySet()) {
            for (Entry<Integer, List<AppLayerInput>> port : host.getValue().entrySet()) {
                InputBundle inputBundle = createInputBundle(port.getValue(), simTime);
                BundleWrapper msg = BundleWrapper.newBuilder().setExecId(execId).setInputBundle(inputBundle).build();
                try {
                    ReqSocket s = new ReqSocket();
                    s.connect("tcp://" + host.getKey() + ":" + port.getKey());

                    // Send message that client is ready to start benchmarking
                    byte[] sendBytes = msg.toByteArray();
                    s.sendBytes(sendBytes);
                    byte[] recv = s.recvBytes(true);
                    output = BundleWrapper.parseFrom(recv);
                }
                catch (IOException ex) {
                    Logger.getLogger(NativeAppManager.class.getName()).log(Level.SEVERE, null, ex);
                }
                catch (java.io.IOException ex) {
                    Logger.getLogger(NativeAppManager.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }
    }

    /**
     * Creates a protobuf object with the app ids and command lines for the given apps.
     * 
     * @param apps The apps.
     * @return The InitBundle.
     */
    private InitBundle createInitBundle(List<NativeApp<?>> apps) {
        InitBundle.Builder ibb = InitBundle.newBuilder();
        for (NativeApp<?> app : apps) {
            ibb.addAppInitConfig(AppInitConfig.newBuilder().setAppId(app.getAppName()).setDevId(1).setCommandLine(app.getCommandLine()));
        }
        return ibb.build();
    }

    /**
     * Creates a protobuf object with the exec id for shutting down.
     * 
     * @return The ShutdownBundle.
     */
    private ShutdownBundle createShutdownBundle() {
        ShutdownBundle.Builder sb = ShutdownBundle.newBuilder();
        return sb.build();
    }

    /**
     * Creates a protobuf object with the app layer inputs for this time step.
     * 
     * @param inputs The inputs.
     * @param simTime The time.
     * @return The InputBundle.
     */
    private InputBundle createInputBundle(List<AppLayerInput> inputs, double simTime) {
        InputBundle.Builder ibb = InputBundle.newBuilder();
        ibb.setSimTime(simTime);

        // This will keep track of which devices an app should be run on.
        // It may make the information slightly more compact, compared to
        // sending a list of apps per device.
        Map<String, List<Long>> appDevMap = new HashMap<String, List<Long>>();

        // This will keep track of messages received by devices. It is used to
        // avoid sending the same message multiple times.
        Map<Long, BasicMessage> txs = new HashMap<Long, BasicMessage>();

        for (AppLayerInput ali : inputs) {
            if (ali.input instanceof RSEDeviceInfo) {
                // Add an RSE device
                addRseDeviceToBundle((RSEDeviceInfo)ali.input, ibb, txs);
            }
            else if (ali.input instanceof OBUDeviceInfo) {
                // Add an OBU device
                addObuDeviceToBundle((OBUDeviceInfo)ali.input, ibb, txs);

            }

            // Add to map of apps to devices
            String appId = ((NativeApp<?>)ali.app).getAppName();
            if (!appDevMap.containsKey(appId)) {
                appDevMap.put(appId, new LinkedList<Long>());
            }
            appDevMap.get(appId).add(ali.input.getDeviceId());
        }

        // Fill out Txs message
        addTxsToBundle(txs, ibb);

        // Fill out AppDevMap message
        addAppDevMapToBundle(appDevMap, ibb);

        return ibb.build();
    }

    /**
     * Converts a map with key: app id, value: list of device ids for devices that run the app, into
     * a protobuf message and adds it to an input bundle.
     * 
     * @param appDevMap The map.
     * @param ibb The input bundle to put the new message in.
     */
    private void addAppDevMapToBundle(Map<String, List<Long>> appDevMap, InputBundle.Builder ibb) {
        for (Entry<String, List<Long>> entry : appDevMap.entrySet()) {
            AppDevMap.Builder admb = AppDevMap.newBuilder();
            admb.setAppId(entry.getKey());
            admb.addAllDevIds(entry.getValue());
            ibb.addAppDevMap(admb);
        }
    }

    /**
     * Converts a map with key: message id, value: message, into a protobuf message and adds it to
     * an input bundle.
     * 
     * @param txs The map.
     * @param ibb The input bundle to put the new message in.
     */
    private void addTxsToBundle(Map<Long, BasicMessage> txs, InputBundle.Builder ibb) {
        for (Entry<Long, BasicMessage> entry : txs.entrySet()) {
            Tx.Builder tb = Tx.newBuilder();
            tb.setMessId(entry.getKey());
            com.harmonia.etexas.wave.NativeAppProtobuf.WSMIndication.Builder wib = com.harmonia.etexas.wave.NativeAppProtobuf.WSMIndication.newBuilder();
            wib.setDest(entry.getValue().getPeerMACAddress()).setSrc(entry.getValue().getOriginMACAddress()).setSize(entry.getValue().getSize());

            try {
                Object newMessage = entry.getValue().getData();
                if (newMessage instanceof Byte[] || (newMessage.getClass().isArray() && newMessage.getClass().getSimpleName().startsWith("byte"))) {
                    wib.setData(ByteString.copyFrom((byte[])newMessage));
                }
                else {
                    byte[] messageAsBytes = SerializationUtils.serialize((Serializable)newMessage);
                    wib.setData(ByteString.copyFrom(messageAsBytes));
                }
                tb.setMess(wib);
                ibb.addMessages(tb);
            }
            catch (Exception e) {
                throw new SimDriverException("Native App Error", "A message to be sent to a native app was not serializable.");
            }
        }
    }

    /**
     * Converts an RSEDeviceInfo into a protobuf message and adds it to an input bundle. Also tracks
     * messages received by the device.
     * 
     * @param rse The device.
     * @param ibb The input bundle.
     * @param txs The map of messages received by devices.
     */
    private void addRseDeviceToBundle(RSEDeviceInfo rse, InputBundle.Builder ibb, Map<Long, BasicMessage> txs) {
        RseDevice.Builder rdb = createRseDeviceMessage(rse);

        // If this is the first RSE device, fill out the RseModelData
        if (!ibb.hasRseData()) {
            ibb.setRseData(createRseModelData(rse.getLaneManagers(), rse.getSignalManagers(), rse.getDetectorManagers()));
        }

        // Process messages
        List<Long> ids = trackMessagesForDevice(rse, txs);
        rdb.addAllMessIds(ids);

        ibb.addRses(rdb.build());
    }

    /**
     * Creates an RSEDevice message from the given RSE.
     * 
     * @param rse The RSE.
     * @return The message.
     */
    private RseDevice.Builder createRseDeviceMessage(RSEDeviceInfo rse) {
        RseDevice.Builder rdb = RseDevice.newBuilder();
        rdb.setDevId(rse.getDeviceId()).setLatitude(rse.getLocation().getX()).setLongitude(rse.getLocation().getY()).setElevation(rse.getLocation().getZ());
        return rdb;
    }

    /**
     * Converts an OBUDeviceInfo to a protobuf message, and adds it to an input bundle. Also tracks
     * messages received by the device.
     * 
     * @param obu The device.
     * @param ibb The input bundle.
     * @param txs The map of messages received by devices.
     */
    private void addObuDeviceToBundle(OBUDeviceInfo obu, InputBundle.Builder ibb, Map<Long, BasicMessage> txs) {
        ObuDevice.Builder odb = createObuDeviceMessage(obu);

        // Process messages
        List<Long> ids = trackMessagesForDevice(obu, txs);
        odb.addAllMessIds(ids);

        ibb.addObus(odb.build());
    }

    /**
     * Creates an OBUDevice message from the given OBU.
     * 
     * @param obu The OBU.
     * @return The message.
     */
    private ObuDevice.Builder createObuDeviceMessage(OBUDeviceInfo obu) {
        ObuDevice.Builder odb = ObuDevice.newBuilder();
        odb.setDevId(obu.getDeviceId());

        // Add Vehicle info
        IVehicle veh = obu.getVehicle();
        VehicleData.Builder vdb = VehicleData.newBuilder();
        vdb.setVehicleId(veh.getVehicleID()).setSpeed(veh.getSpeed()).setAcceleration(veh.getAcceleration()).setLength(veh.getLength()).setWidth(veh.getWidth()).setLaneId(veh.getLaneID())
                .setHeading(veh.getHeading()).setHeight(veh.getHeight()).setLatitude(veh.getLatitude()).setLongitude(veh.getLongitude()).setElevation(veh.getElev())
                .setBrakePressed(veh.isBrakePressed()).setX(veh.getX()).setY(veh.getY()).setZ(veh.getZ());
        if (veh.getType().equals(Vehicle.VEHICLE_TYPE.CAR)) {
            vdb.setType(VehicleType.CAR);
        }
        else if (veh.getType().equals(Vehicle.VEHICLE_TYPE.BUS)) {
            vdb.setType(VehicleType.BUS);
        }
        else {
            vdb.setType(VehicleType.TRACTOR_TRAILER);
        }
        odb.setVehicle(vdb);
        return odb;
    }

    /**
     * Creates a protocol buffers object from the specified manager mappings.
     * 
     * @param lm The map of intersection lane managers.
     * @param sm The map of intersection signal managers.
     * @param dm The map of intersection detector managers.
     * @return The <code>RseModelData</code> for protocol buffers.
     */
    private RseModelData createRseModelData(Map<Integer, ILaneManager> lm, Map<Integer, ISignalManager> sm, Map<Integer, IDetectorManager> dm) {
        RseModelData.Builder rmd = RseModelData.newBuilder();

        for (Entry<Integer, ISignalManager> signalManagerEntry : sm.entrySet()) {

            SignalManagerEntry.Builder signalBuilder = SignalManagerEntry.newBuilder();
            signalBuilder.setKey(signalManagerEntry.getKey());
            signalBuilder.setValue(createSignalManagerData(signalManagerEntry.getValue()));
            rmd.addSignalMap(signalBuilder);
        }

        for (Entry<Integer, ILaneManager> laneManagerEntry : lm.entrySet()) {

            LaneManagerEntry.Builder laneBuilder = LaneManagerEntry.newBuilder();
            laneBuilder.setKey(laneManagerEntry.getKey());
            laneBuilder.setValue(createLaneManagerData(laneManagerEntry.getValue()));
            rmd.addLaneMap(laneBuilder);
        }

        for (Entry<Integer, IDetectorManager> detectorManagerEntry : dm.entrySet()) {

            DetectorManagerEntry.Builder detectorBuilder = DetectorManagerEntry.newBuilder();
            detectorBuilder.setKey(detectorManagerEntry.getKey());
            detectorBuilder.setValue(createDetectorManagerData(detectorManagerEntry.getValue()));
            rmd.addDetectorMap(detectorBuilder);
        }

        return rmd.build();
    }

    /**
     * Creates a protobuf object from the given signal manager.
     * 
     * @param sm The signal manager.
     * @return The SignalManagerData.
     */
    private SignalManagerData createSignalManagerData(ISignalManager sm) {
        SignalManagerData.Builder smb = SignalManagerData.newBuilder();

        for (ISignalIndication si : sm) {
            SignalManagerData.SignalIndicationData.Builder sib = SignalManagerData.SignalIndicationData.newBuilder();
            sib.setLaneId(si.getLaneId()).setTimeToChange(si.getTimeToChange());

            SignalManagerData.SignalIndicationData.Color c;
            switch (si.getColorIndication()) {
                case GREEN:
                    c = SignalManagerData.SignalIndicationData.Color.GREEN;
                    break;
                case YELLOW:
                    c = SignalManagerData.SignalIndicationData.Color.YELLOW;
                    break;
                case RED:
                    c = SignalManagerData.SignalIndicationData.Color.RED;
                    break;
                case NONE:
                    c = SignalManagerData.SignalIndicationData.Color.NONE;
                    break;
                default:
                    c = SignalManagerData.SignalIndicationData.Color.NONE;
                    break;
            }

            SignalManagerData.SignalIndicationData.Type t;
            switch (si.getTypeIndication()) {
                case BALL:
                    t = SignalManagerData.SignalIndicationData.Type.BALL;
                    break;
                case LEFT_ARROW:
                    t = SignalManagerData.SignalIndicationData.Type.LEFT_ARROW;
                    break;
                case RIGHT_ARROW:
                    t = SignalManagerData.SignalIndicationData.Type.RIGHT_ARROW;
                    break;
                case STRAIGHT_ARROW:
                    t = SignalManagerData.SignalIndicationData.Type.STRAIGHT_ARROW;
                    break;
                case UTURN_ARROW:
                    t = SignalManagerData.SignalIndicationData.Type.UTURN_ARROW;
                    break;
                case STOP_SIGN:
                    t = SignalManagerData.SignalIndicationData.Type.STOP_SIGN;
                    break;
                case YIELD_SIGN:
                    t = SignalManagerData.SignalIndicationData.Type.YIELD_SIGN;
                    break;
                case UNCONTROLLED:
                    t = SignalManagerData.SignalIndicationData.Type.UNCONTROLLED;
                    break;
                case UNKNOWN:
                    t = SignalManagerData.SignalIndicationData.Type.UNKNOWN;
                    break;
                default:
                    t = SignalManagerData.SignalIndicationData.Type.UNKNOWN;
                    break;
            }

            SignalManagerData.SignalIndicationData.State s;
            switch (si.getStateIndication()) {
                case STEADY:
                    s = SignalManagerData.SignalIndicationData.State.STEADY;
                    break;
                case FLASHING:
                    s = SignalManagerData.SignalIndicationData.State.FLASHING;
                    break;
                case SOFT:
                    s = SignalManagerData.SignalIndicationData.State.SOFT;
                    break;
                default:
                    s = SignalManagerData.SignalIndicationData.State.STEADY;
                    break;
            }

            sib.setColor(c).setType(t).setState(s);
            smb.addSignalIndications(sib);
        }

        return smb.build();
    }

    /**
     * Creates a protobuf object from the given detector manager.
     * 
     * @param dm The detector manager.
     * @return The DetectorManagerData.
     */
    private DetectorManagerData createDetectorManagerData(IDetectorManager dm) {
        DetectorManagerData.Builder dmb = DetectorManagerData.newBuilder();

        for (IDetector det : dm) {
            DetectorManagerData.DetectorData.Builder db = DetectorManagerData.DetectorData.newBuilder();

            db.setDetectorId(det.getDetectorID()).setPresenceDetectCap(det.isPresenceDetectCap()).setPulseDetectCap(det.isPulseDetectCap()).setSpeedDetectCap(det.isSpeedDetectCap())
                    .setLengthDetectCap(det.isLengthDetectCap()).addAllLaneIds(det.getLaneIDs());

            IDetectorEvent de = det.getDetEvent();
            if (de != null) {
                db.setDetEvent(
                        DetectorEventData.newBuilder().setDetectorId(det.getDetectorID()).setPulse(de.getPulse()).setPresence(de.isPresence()).setSpeed(de.getSpeed()).setLength(de.getLength()));
            }

            Polygon poly = det.getArea();
            if (poly != null) {
                PolygonData.Builder pdb = PolygonData.newBuilder();
                pdb.setNpoints(poly.npoints);
                for (int k = 0; k < poly.npoints; ++k) {
                    pdb.addXpoints(poly.xpoints[k]);
                    pdb.addYpoints(poly.ypoints[k]);
                }
                db.setArea(pdb);
            }

            dmb.addDetectors(db);
        }

        return dmb.build();
    }

    /**
     * Creates a protobuf object from the given lane manager.
     * 
     * @param lm The lane manager.
     * @return The LaneManagerData.
     */
    private LaneManagerData createLaneManagerData(ILaneManager lm) {
        LaneManagerData.Builder lmb = LaneManagerData.newBuilder();

        lmb.setLatitude(lm.getLatitude()).setLongitude(lm.getLongitude()).setElevation(lm.getElevation()).setIntersectionId(lm.getIntersectionId()).setGeoCalculatorType(lm.getGeoCalculatorType());

        for (ILane lane : lm) {
            LaneData.Builder lb = LaneData.newBuilder();
            lb.setLaneId(lane.getLaneId()).setApproachId(lane.getApproachId()).setSpeedLimit(lane.getSpeedLimitInMetersPerSecond());

            LaneData.Type t;
            if (lane.getType().equals(Lane.INBOUND)) {
                t = LaneData.Type.INBOUND;
            }
            else if (lane.getType().equals(Lane.OUTBOUND)) {
                t = LaneData.Type.OUTBOUND;
            }
            else {
                t = LaneData.Type.UNSET;
            }
            lb.setType(t);

            for (ILaneNode node : lane.getLaneGeomList()) {
                lb.addLaneGeomList(LaneNode.newBuilder().setX(node.getX()).setY(node.getY()).setZ(node.getZ()).setWidth(node.getWidth()));
            }

            for (ILaneMovement mov : lane.getLaneMovements().values()) {
                LaneData.LaneMovement.Movement m;
                switch (mov.getMovement()) {
                    case LEFT_TURN:
                        m = Movement.LEFT_TURN;
                        break;
                    case RIGHT_TURN:
                        m = Movement.RIGHT_TURN;
                        break;
                    case STRAIGHT:
                        m = Movement.STRAIGHT;
                        break;
                    case RIGHT_TURN_ON_RED:
                        m = Movement.RIGHT_TURN_ON_RED;
                        break;
                    case LEFT_TURN_ON_RED:
                        m = Movement.LEFT_TURN_ON_RED;
                        break;
                    case U_TURN:
                        m = Movement.U_TURN;
                        break;
                    default:
                        m = Movement.STRAIGHT;
                }
                lb.addLaneMovements(LaneMovement.newBuilder().setMovement(m).setMovementId(mov.getMovementId()));
            }

            lmb.addLanes(lb);
        }

        return lmb.build();
    }

    /**
     * Updates the map of messages and returns the keys for the messages received by the given
     * device.
     * 
     * @param dev The device.
     * @param txs The map of messages to be sent to the agent.
     * @return The keys for messages received by the device.
     */
    private List<Long> trackMessagesForDevice(IDeviceInfo dev, Map<Long, BasicMessage> txs) {
        List<Long> ret = new ArrayList<Long>(dev.getMessages().size());

        for (BasicMessage message : dev.getMessages()) {
            Long key = null;
            // Check if this message has already been seen
            for (Entry<Long, BasicMessage> entry : txs.entrySet()) {
                if (entry.getValue().equals(message)) {
                    key = entry.getKey();
                    break;
                }
            }
            // If this is the first time seen, add to txs
            if (key == null) {
                key = currMessId;
                currMessId++;
                txs.put(key, message);
            }
            ret.add(key);
        }

        return ret;
    }
}
