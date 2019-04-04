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

import java.awt.Polygon;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import org.etexascode.CoberturaIgnore;
import org.etexascode.appslayerdata.AppLayerInput;
import org.etexascode.appslayerdata.AppLayerOutput;
import org.etexascode.appslayerdata.AppShutdownLayerInput;
import org.etexascode.appslayerdata.CellularDeviceInfo;
import org.etexascode.appslayerdata.IDeviceInfo;
import org.etexascode.appslayerdata.OBUDeviceInfo;
import org.etexascode.appslayerdata.RSEDeviceInfo;
import org.etexascode.appslayerdata.RemoteProxyApp;
import org.etexascode.appslayerdata.ReportDeviceInfo;
import org.etexascode.datalayer.IDataLayer;
import org.etexascode.datalayer.interfaces.ICommandsComponent;
import org.etexascode.datalayer.interfaces.IDevicesComponent;
import org.etexascode.datalayer.interfaces.IIntersectionModelsComponent;
import org.etexascode.datalayer.interfaces.IMacManagerComponent;
import org.etexascode.datalayer.interfaces.IMessageComponent;
import org.etexascode.datalayer.interfaces.IVehicleIdComponent;
import org.etexascode.devicedata.AbstractEmbeddedDeviceData;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.CellDeviceData;
import org.etexascode.devicedata.DualIntIdentifier;
import org.etexascode.devicedata.FixedCellDeviceData;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.devicedata.LogData;
import org.etexascode.devicedata.OBUDeviceData;
import org.etexascode.devicedata.RSEDeviceData;
import org.etexascode.devicedata.ReportDeviceData;
import org.etexascode.interrep.datamodel.Command;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.persistencelayer.IPersistenceLayer;
import org.etexascode.wavesim.Rx;
import org.etexascode.wavesim.Tx;

/**
 * An in memory, single intersection implementation of the data layer.
 * 
 * @author ablatt
 * @author ttevendale
 */
public class SingleIntersectionDataLayer implements IDataLayer {

    /**
     * Container for keeping track of interReps
     */
    IIntersectionModelsComponent interRepContainer = new SingleIntersectionModelsComponent();

    /**
     * Container for keeping track of commands
     */
    ICommandsComponent commandsContainer = new SingleIntersectionCommandsComponent();

    /**
     * Container for keeping track of messages
     */
    IMessageComponent messageContainer = new SingleIntersectionMessageComponent();

    /**
     * Container for keeping track of devices
     */
    IDevicesComponent devicesContainer = null;

    /**
     * Container for keeping track of macs
     */
    IMacManagerComponent macContainer = null;

    /**
     * Persistence layer for putting app logs and commands.
     */
    IPersistenceLayer persistenceLayer = null;

    /** The vehicle ID component */
    private IVehicleIdComponent vehicleIdComponent;

    /**
     * Random seed (mostly used by the mac container)
     */
    int randomSeed = 42;

    /**
     * Default DualIntIdentifier (a commonly used object)
     */
    final DualIntIdentifier dii = new DualIntIdentifier(0, 0, true);

    /**
     * Scalar used to go between step num and sim time
     */
    double stepScalar;

    /**
     * Container for DistanceImpl
     */
    static final DistanceImpl reportDeviceLocation = new DistanceImpl(0, 0, 0);

    /**
     * Constructor.
     * 
     * @param devs RSE, Fixed Cell, and Report Devices to initialize the data layer.
     * @param logs The persistence layer for log storage.
     * @param randomSeed The random seed (used by the mac manager).
     * @param vehicleIdComponent The vehicle ID component.
     */
    public SingleIntersectionDataLayer(List<IDeviceData> devs, IPersistenceLayer logs, int randomSeed, IVehicleIdComponent vehicleIdComponent) {
        macContainer = new SingleIntersectionMacManagerComponent(new Random(randomSeed).nextInt(), devs);
        for (IDeviceData dev : devs) {
            dev.setMacAddress(macContainer.getMac(0, dev.getProperId()));
        }
        devicesContainer = new SingleIntersectionDeviceComponent(devs);
        persistenceLayer = logs;
        this.randomSeed = randomSeed;
        this.vehicleIdComponent = vehicleIdComponent;
    }

    /**
     * Sets the step size
     * 
     * @param stepSize the step size to be set to.
     */
    public void setStepSize(double stepSize) {
        stepScalar = stepSize;
    }

    /**
     * Gets the interrep information model, if null it creates a new model
     * 
     * @param stepNum The current step number
     * @param interRepId The interrep id for the simulation
     * @return The interrep information model
     */
    @Override
    @CoberturaIgnore
    public InterRepInfoModel getInfoModel(int stepNum, int interRepId) {
        if (interRepContainer == null) {
            return new InterRepInfoModel(new LaneManager(), new VehicleManager(), new SignalManager(), new DetectorManager(), new ReferencePoint[0], -1.0, 0.0);
        }
        return interRepContainer.getInfoModel(stepNum, interRepId);
    }

    /**
     * Gets the vehicle manager information
     * 
     * @param stepNum The current step number
     * @param interRepId The interrep id for the simulation
     * @return Vehicle information for the interrep
     */
    @Override
    @CoberturaIgnore
    public IVehicleManager getVehicleManagerInfo(int stepNum, int interRepId) {
        return interRepContainer.getVehicles(stepNum, interRepId);
    }

    /**
     * Gets a string of injected vehicles for the step number and interrep id
     * 
     * @param stepNum The current step number
     * @param interRepId The interrep id from the simulation
     * @return Hashset string of injected vehicles
     */
    @Override
    @CoberturaIgnore
    public Set<String> getInjectedVehicles(int stepNum, int interRepId) {
        return new HashSet<String>();
    }

    /**
     * Gets the random seed
     * 
     * @param stepNum The current step number
     * @param interRepId The interrep id for the simulation
     * @return randomSeed The random seed
     */
    @Override
    @CoberturaIgnore
    public int getRandomSeed(int stepNum, int interRepId) {
        return randomSeed;
    }

    /**
     * Sets the random seed
     * 
     * @param stepNum The current step number
     * @param interRepId The interrep id from the simulation
     * @param randomSeed The random seed to be set
     */
    @Override
    @CoberturaIgnore
    public void putRandomSeed(int stepNum, int interRepId, int randomSeed) {
        this.randomSeed = randomSeed;
    }

    /**
     * Sets the application device data for the MAC and devices containers
     * 
     * @param stepNum The current step number
     * @param deviceData The list of on board unit device data
     */
    @Override
    @CoberturaIgnore
    public void putAppDeviceData(int stepNum, List<? extends IDeviceData> deviceData) {
        macContainer.putNewDevices(stepNum, deviceData);
        devicesContainer.putNewDevices(stepNum, deviceData);
    }

    /**
     * Sets the logged out vehicles for the devices container
     * 
     * @param stepNum The current step number
     * @param logoutVehicles The vehicles to log out of the devices container
     */
    @Override
    @CoberturaIgnore
    public void putLoggedOutVehicles(int stepNum, Map<DualIntIdentifier, List<IVehicle>> logoutVehicles) {
        devicesContainer.putLogoutVehicles(stepNum, logoutVehicles.get(dii));
    }

    /**
     * Gets the simulation time from the step number and the step scalar
     * 
     * @param stepNum The current step number
     * @return The simulation time
     */
    @Override
    @CoberturaIgnore
    public double getSimTime(int stepNum) {
        return stepNum * stepScalar;
    }

    /**
     * Sets the interrep model information to its container
     * 
     * @param stepNum The current step number
     * @param interRepId The interrep id from the simulation
     * @param interRep The interrep information model to be added to the container
     */
    @Override
    @CoberturaIgnore
    public void putInterRepModel(int stepNum, int interRepId, InterRepInfoModel interRep) {
        interRepContainer.putInterRepInfoModel(stepNum, interRepId, interRep);
    }

    /**
     * Gets the signal commands from the commands container
     * 
     * @param stepNum The current step number
     * @param interRepId The interrep id from the simulation
     * @return The signal commands
     */
    @Override
    @CoberturaIgnore
    public List<SignalCommand> getSignalCommands(int stepNum, int interRepId) {
        return commandsContainer.getSignalCommands(stepNum, interRepId);
    }

    /**
     * Gets the vehicle commands from the commands container
     * 
     * @param stepNum The current step number
     * @param interRepId The interrep id from the simulation
     * @return The vehicle commands
     */
    @Override
    @CoberturaIgnore
    public List<VehicleCommand> getVehicleCommands(int stepNum, int interRepId) {
        return commandsContainer.getVehicleCommands(stepNum, interRepId);
    }

    /**
     * Gets the vehicle injection requests from the commands container
     * 
     * @param stepNum The current step number
     * @param interRepId The interrep id from the simulation
     * @return The vehicle injection requests
     */
    @Override
    @CoberturaIgnore
    public List<VehicleInjectionRequest> getVehicleInjectionRequests(int stepNum, int interRepId) {
        return commandsContainer.getVehicleInjections(stepNum, interRepId);
    }

    /**
     * Gets the application message data and produces inputs
     * 
     * @param stepNum The current step number
     * @return The application message data inputs
     */
    @Override
    @CoberturaIgnore
    public List<AppLayerInput> getAppData(int stepNum) {
        Map<Long, List<BasicMessage>> stepMesses = messageContainer.getIndicationsForDevicesByTimeStep(stepNum);
        return genInputs(stepNum, stepMesses, devicesContainer.getActiveDevices(stepNum), macContainer);
    }

    /**
     * Generate App Layer inputs
     * 
     * @param stepNum The current step number
     * @param stepMesses The messages to be received by devices this time step
     * @param activeDevices The devices that are active this time step
     * @param macContainer The mac container to get device macs from
     * @return The inputs to the app layer
     */
    List<AppLayerInput> genInputs(int stepNum, Map<Long, List<BasicMessage>> stepMesses, Iterable<IDeviceData> activeDevices, IMacManagerComponent macContainer) {
        List<AppLayerInput> ret = new LinkedList<AppLayerInput>();

        // Collect all messages received in this step to pass to the report device.
        List<BasicMessage> rxMessages = new LinkedList<BasicMessage>();
        for (List<BasicMessage> messageList : stepMesses.values()) {
            if (messageList != null) {
                rxMessages.addAll(messageList);
            }
        }

        // Now iterate over all active devices and perform app input logic.
        for (IDeviceData idd : activeDevices) {
            IDeviceInfo devInf = null;
            long mac = macContainer.getMac(stepNum, idd.getProperId());
            idd.setMacAddress(mac);
            if (idd instanceof ReportDeviceData) {

                Map<Integer, IVehicleManager> vehicleMap = new HashMap<Integer, IVehicleManager>();
                Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>();
                Map<Integer, ILaneManager> laneMap = new HashMap<Integer, ILaneManager>();
                Map<Integer, IDetectorManager> detectorMap = new HashMap<Integer, IDetectorManager>();

                for (int intersection : ((ReportDeviceData)idd).getIntersections()) {

                    InterRepInfoModel irim = interRepContainer.getInfoModel(stepNum, intersection);
                    vehicleMap.put(intersection, irim.vmi);
                    signalMap.put(intersection, irim.smi);
                    laneMap.put(intersection, irim.lmi);
                    detectorMap.put(intersection, irim.dmi);
                }

                Collection<BasicMessage> wsmIndications = new ArrayList<BasicMessage>();
                for (List<BasicMessage> messages : messageContainer.getTxMessages(stepNum).values()) {
                    wsmIndications.addAll(messages);
                }

                devInf = new ReportDeviceInfo(vehicleMap, signalMap, laneMap, detectorMap, wsmIndications, rxMessages, mac, reportDeviceLocation);
            }
            else {
                List<BasicMessage> inds = stepMesses.get(mac);
                if (inds == null) {
                    inds = new ArrayList<BasicMessage>(0);
                }
                if (idd instanceof OBUDeviceData) {
                    devInf = new OBUDeviceInfo(interRepContainer.getVehicleInfoById(stepNum, ((AbstractEmbeddedDeviceData)idd).getVehicleId()), inds, mac);
                }
                else if (idd instanceof CellDeviceData) {
                    IDistanceable location = interRepContainer.getVehicleInfoById(stepNum, ((AbstractEmbeddedDeviceData)idd).getVehicleId());
                    devInf = new CellularDeviceInfo(location, inds, mac);
                }
                else if (idd instanceof RSEDeviceData) {

                    Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>();
                    Map<Integer, ILaneManager> laneMap = new HashMap<Integer, ILaneManager>();
                    Map<Integer, IDetectorManager> detectorMap = new HashMap<Integer, IDetectorManager>();

                    for (int intersection : ((RSEDeviceData)idd).getIntersections()) {

                        InterRepInfoModel irim = interRepContainer.getInfoModel(stepNum, intersection);
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

    /**
     * Sets the application outputs for signals and vehicles into the log container
     * 
     * @param stepNum The current step number
     * @param appOutputs The list of application layer outputs to add to the logs
     */
    @Override
    public void putAppOutputs(int stepNum, List<AppLayerOutput> appOutputs) {
        messageContainer.putAppLayerOutputs(stepNum, appOutputs);
        List<LogData> logs = new LinkedList<LogData>(); // Note: ablatt - is this faster than just
                                                        // adding the logs straight to the
                                                        // persistence layer in practice?
        for (AppLayerOutput alo : appOutputs) {
            commandsContainer.putSignalCommands(stepNum, 0, alo.getSigCommands()); // Note: this
                                                                                   // means that
                                                                                   // this cannot be
                                                                                   // used for
                                                                                   // multiple
                                                                                   // intersections...
            commandsContainer.putVehicleCommands(stepNum, 0, alo.getVehCommands());
            persistenceLayer.persistSignalCommands(alo.appName, 0, stepNum * stepScalar, alo.getSigCommands());
            persistenceLayer.persistVehicleCommands(alo.appName, 0, stepNum * stepScalar, alo.getVehCommands());
            logs.addAll(alo.getLogs());
        }
        persistenceLayer.persistAppLogs(logs);
    }

    /**
     * Gets the transmission from the wave simulation inputs and returns them in a message container
     * 
     * @param stepNum The current step number
     * @param segment The polygon representation of the message area
     * @return The message container with the transmissions
     */
    @Override
    @CoberturaIgnore
    public Iterable<Tx> getTxs(int stepNum, Polygon segment) {
        return messageContainer.getWaveSimInputs(stepNum, segment);
    }

    /**
     * Sets the transmission of messages from nodes to the message center
     * 
     * @param stepNum The current step number
     * @param nodes The connection of transmissions sent
     */
    @Override
    @CoberturaIgnore
    public void putMessages(int stepNum, Collection<Rx> nodes) {
        messageContainer.putWaveSimOutputs(stepNum, nodes);
    }

    /**
     * Gets the step size for the simulation
     * 
     * @return stepScalar The step size
     */
    @Override
    public double getStepSize() {
        return stepScalar;
    }

    /**
     * Sets application log information to the log container
     * 
     * @param stepNum The current step number
     * @param logs The list of log application log data to add to the container
     */
    @Override
    public void putAppLogs(int stepNum, List<LogData> logs) {
        persistenceLayer.persistAppLogs(logs);
    }

    /**
     * Gets the applications to shut down this time step
     * 
     * @param stepNum The current step number
     * @return The stopped devices
     */
    @Override
    @CoberturaIgnore
    public Iterable<AppShutdownLayerInput> getAppsToShutdown(int stepNum) {
        return getShutdownInputs(devicesContainer.getStoppedDevices(stepNum));
    }

    /**
     * Shuts down all applications
     * 
     * @return The list of all applications for shut down
     */
    @Override
    @CoberturaIgnore
    public Iterable<AppShutdownLayerInput> getAllAppsForShutdown() {
        return getShutdownInputs(devicesContainer.stopAllDevices());
    }

    /**
     * Gets a table of all the vehicles from the interrep container
     * 
     * @param stepNum The current step number
     * @param interRepId The interrep id for the simulation
     * @return The vehicles
     */
    @Override
    @CoberturaIgnore
    public Iterable<IVehicle> getAllVehiclesForTable(int stepNum, int interRepId) {
        if (interRepContainer == null) {
            return new ArrayList<IVehicle>(0);
        }
        return interRepContainer.getVehicles(stepNum, interRepId);
    }

    /**
     * Gets a list of the devices that are currently active in the simulation.
     * 
     * @param stepNum The current step number
     * @return The current devices
     */
    @Override
    public Iterable<? extends IDeviceData> getCurrentDevices(int stepNum) {
        return devicesContainer.getActiveDevices(stepNum);
    }

    @Override
    public Map<Long, List<BasicMessage>> getRxMessages(int stepNum) {
        return messageContainer.getIndicationsForDevicesByTimeStep(stepNum);
    }

    @Override
    public Map<Long, List<BasicMessage>> getTxMessages(int stepNum) {
        return messageContainer.getTxMessages(stepNum);
    }

    /**
     * Gets the lane information from a vehicle
     * 
     * @param stepNum The current step number
     * @param vi The vehicle from which to get the lane information
     * @return The lane information
     */
    @Override
    @CoberturaIgnore
    public ILane getLaneFromVehicle(int stepNum, IVehicle vi) {
        return interRepContainer.getLaneInfoByVehicle(stepNum, vi);
    }

    /**
     * Gets the signal indication information for a lane
     * 
     * @param stepNum The current step number
     * @param li The lane from which to get the information
     * @return The signal information
     */
    @Override
    @CoberturaIgnore
    public List<? extends ISignalIndication> getSignalIndication(int stepNum, ILane li) {
        return interRepContainer.getSignalsByLane(stepNum, li);
    }

    /**
     * Adds signal commands to the commands container
     * 
     * @param source The source of the command.
     * @param stepNum The current step number.
     * @param command The signal command to be added.
     * @param interRepId The interrep id for the simulation.
     */
    @Override
    @CoberturaIgnore
    public void addSignalCommand(String source, int stepNum, SignalCommand command, int interRepId) {
        commandsContainer.putSignalCommand(stepNum, interRepId, command);
        persistenceLayer.persistSignalCommands(source, interRepId, stepNum * stepScalar, Arrays.asList(command));
    }

    /**
     * Adds a vehicle command to the commands container
     * 
     * @param source The source of the command.
     * @param stepNum The current step number.
     * @param command The vehicle command to be added.
     * @param interRepId The interrep id for the simulation.
     */
    @Override
    @CoberturaIgnore
    public void addVehicleCommand(String source, int stepNum, VehicleCommand command, int interRepId) {
        commandsContainer.putVehicleCommand(stepNum, interRepId, command);
        persistenceLayer.persistVehicleCommands(source, interRepId, stepNum * stepScalar, Arrays.asList(command));
    }

    @Override
    @CoberturaIgnore
    public void addVehicleInjectionRequest(String source, int stepNum, VehicleInjectionRequest request, int interRepId) {
        commandsContainer.putVehicleInjection(stepNum, interRepId, request);
        persistenceLayer.persistVehicleInjectionRequests(source, interRepId, stepNum * stepScalar, Arrays.asList(request));
    }

    /**
     * Gets a list of application devices that are started
     * 
     * @param stepNum The current step number
     * @return The list of started devices
     */
    @Override
    @CoberturaIgnore
    public Iterable<IDeviceData> getStartedAppDevices(int stepNum) {
        if (devicesContainer == null) {
            return new ArrayList<IDeviceData>(0);
        }
        return devicesContainer.getNewDevices(stepNum);
    }

    /**
     * Gets a list of application devices that are stopped
     * 
     * @param stepNum The current step number
     * @return The list of stopped devices
     */
    @Override
    @CoberturaIgnore
    public Iterable<IDeviceData> getStoppedAppDevices(int stepNum) {
        if (devicesContainer == null) {
            return new ArrayList<IDeviceData>(0);
        }
        return devicesContainer.getStoppedDevices(stepNum);
    }

    /**
     * Gets the started remote apps.
     * 
     * @param timestep The timestep.
     * @return List of started remote apps.
     */
    @Override
    public List<RemoteProxyApp<?>> getStartedRemoteApps(int timestep) {
        Iterable<IDeviceData> start = getStartedAppDevices(timestep);
        List<RemoteProxyApp<?>> started = new ArrayList<RemoteProxyApp<?>>();
        Iterator<IDeviceData> iter = start.iterator();
        while (iter.hasNext()) {
            IDeviceData idd = iter.next();
            List<IConnectedVehicleApp<?>> apps = idd.getApps();
            for (IConnectedVehicleApp<?> app : apps) {
                if (app instanceof RemoteProxyApp) {
                    RemoteProxyApp<?> apa = (RemoteProxyApp<?>)app;
                    started.add(apa);
                }
            }
        }
        return started;
    }

    /**
     * Gets the stopped remote apps.
     * 
     * @param timestep The timestep.
     * @return The stopped remote apps.
     */
    @Override
    public List<RemoteProxyApp<?>> getStoppedRemoteApps(int timestep) {
        Iterable<IDeviceData> stop = getStoppedAppDevices(timestep);
        List<RemoteProxyApp<?>> stopped = new ArrayList<RemoteProxyApp<?>>();
        Iterator<IDeviceData> iter = stop.iterator();

        while (iter.hasNext()) {
            IDeviceData idd = iter.next();
            List<IConnectedVehicleApp<?>> apps = idd.getApps();
            for (IConnectedVehicleApp<?> app : apps) {
                if (app instanceof RemoteProxyApp) {
                    RemoteProxyApp<?> apa = (RemoteProxyApp<?>)app;
                    stopped.add(apa);
                }
            }
        }
        return stopped;
    }

    /**
     * Shuts down the data layer
     */
    @Override
    @CoberturaIgnore
    public void shutdown() {
        interRepContainer = null;
        commandsContainer = null;
        messageContainer = null;
        devicesContainer = null;
    }

    /**
     * Convert Devices to Shut Down Layer inputs
     * 
     * @param stopped The devices to be stopped
     * @return The inputs for the shutdown layer
     */
    List<AppShutdownLayerInput> getShutdownInputs(Iterable<IDeviceData> stopped) {
        LinkedList<AppShutdownLayerInput> ret = new LinkedList<AppShutdownLayerInput>();

        for (IDeviceData idd : stopped) {
            for (IConnectedVehicleApp<?> ica : idd.getApps()) {
                ret.add(new AppShutdownLayerInput(macContainer.getMac(0, idd.getProperId()), ica));
            }
        }

        return ret;
    }

    @Override
    @CoberturaIgnore
    public Iterable<? extends IDetector> getAllDetectors(int stepNum, int interRepId) {
        return interRepContainer.getDetectors(stepNum, interRepId);
    }

    @Override
    @CoberturaIgnore
    public Collection<? extends Command> peekSignalCommands(int stepNum, int interRepId) {
        // return commands container current signal commands list
        return commandsContainer.peekSignalCommands(stepNum, interRepId);
    }

    @Override
    @CoberturaIgnore
    public Collection<? extends Command> peekVehicleCommands(int stepNum, int interRepId) {
        // return commands container current vehicle commands
        return commandsContainer.peekVehicleCommands(stepNum, interRepId);
    }

    @Override
    @CoberturaIgnore
    public Collection<? extends Command> peekVehicleInjections(int stepNum, int interRepId) {
        // return commands container current injected vehicles
        return commandsContainer.peekVehicleInjections(stepNum, interRepId);

    }

    @Override
    public String getProperVehicleId(long globalId) {

        return vehicleIdComponent.getProperVehicleId(globalId);
    }

    @Override
    public Long getGlobalVehicleId(String properId) {

        return vehicleIdComponent.getGlobalVehicleId(properId);
    }

    @Override
    public long addVehicleId(IVehicle vehicle) {

        return vehicleIdComponent.addVehicleId(vehicle);
    }

    @Override
    public int nextInjectedVehicleId() {

        return vehicleIdComponent.nextInjectedVehicleId();
    }

    @Override
    public void putReturnedVehicle(IDable vehicle) {

        vehicleIdComponent.putReturnedVehicle(vehicle);
    }

    @Override
    public void putStalledVehicle(IDable oldVehicle, IDable newVehicle) {

        vehicleIdComponent.putStalledVehicle(oldVehicle, newVehicle);
    }

    @Override
    public boolean isStalledVehicle(IDable vehicle) {

        return vehicleIdComponent.isStalledVehicle(vehicle);
    }
}
