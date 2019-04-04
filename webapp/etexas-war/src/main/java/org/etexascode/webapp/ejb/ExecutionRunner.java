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
package org.etexascode.webapp.ejb;

import java.io.UnsupportedEncodingException;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.annotation.Resource;
import javax.ejb.Remove;
import javax.ejb.Stateful;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import org.etexascode.appslayer.IRemoteAppManager;
import org.etexascode.cellular.BasicCellMessage;
import org.etexascode.datalayer.interfaces.IWebAppDataLayer;
import org.etexascode.devicedata.AbstractEmbeddedDeviceData;
import org.etexascode.devicedata.AppInitConfig;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.FixedCellDeviceData;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.devicedata.OBUDeviceData;
import org.etexascode.devicedata.RSEDeviceData;
import org.etexascode.driver.ComponentContainer;
import org.etexascode.driver.IDriver;
import org.etexascode.driver.SimDriverException;
import org.etexascode.driver.singleintersection.DefaultComponentCreator;
import org.etexascode.driver.singleintersection.SingleIntersectionDriver;
import org.etexascode.interrep.datamodel.CommandSource;
import org.etexascode.interrep.datamodel.ExecMetaData;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.etexascode.interrep.datamodel.SimulatorMessage;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.interrep.datamodel.VehicleLaneChangeCommand;
import org.etexascode.interrep.datamodel.VehicleSpeedCommand;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.IFormattedMessage;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.topography.ITopography;
import org.etexascode.j2735.BasicSafetyMessage;
import org.etexascode.j2735.BasicSafetyMessageVerbose;
import org.etexascode.j2735.CommonSafetyRequest;
import org.etexascode.j2735.MapData;
import org.etexascode.j2735.SPAT;
import org.etexascode.nonstd.DetectorData;
import org.etexascode.nonstd.SimData;
import org.etexascode.wavesim.CellTower;
import org.etexascode.wavesim.CellularConfig;
import org.etexascode.webapp.datamodel.Composite;
import org.etexascode.webapp.datamodel.Execution;
import org.etexascode.webapp.datamodel.ExecutionMessage;
import org.etexascode.webapp.datamodel.ExecutionMessageType;
import org.etexascode.webapp.datamodel.ExecutionStatus;
import org.etexascode.webapp.datamodel.MessageData;
import org.etexascode.webapp.exception.WebAppException;
import org.etexascode.webapp.ra.api.SimFactory;
import org.etexascode.webapp.rest.model.OnBoardDevice;
import org.etexascode.webapp.rest.model.OnBoardDevice.OnBoardDeviceType;
import org.etexascode.webapp.rest.model.StandaloneDevice;
import org.etexascode.webapp.rest.model.StandaloneDevice.StandaloneDeviceType;
import org.slf4j.LoggerFactory;

import com.harmonia.etexas.wave.NativeAppManager;

/**
 * The EJB to manage a running execution.
 * 
 * @author bbadillo
 * @author ablatt
 * @author jrutherford
 * @author dranker
 * @author ttevendale
 * @author emyers
 */
@Stateful
public class ExecutionRunner {

    /** The finished status of the execution. */
    private boolean isFinished;

    /** The error status of the underlying simulators */
    private boolean hasSimulatorError;

    /** The database entity manager. */
    @PersistenceContext(unitName = "etexas-pu")
    private EntityManager entityManager;

    /** The running execution. */
    private Execution execution;

    /** The simulation driver. */
    private IDriver driver;

    /** The current step count. */
    private int stepCount;

    /** The map of information models. */
    private Map<Long, InterRepInfoModel> modelMap = new HashMap<Long, InterRepInfoModel>();

    /** The remote application manager. */
    private IRemoteAppManager remoteManager;

    /** The data layer. */
    private IWebAppDataLayer dataLayer;

    /** The list of current transmitted messages. */
    private Map<Long, List<BasicMessage>> txMessageMap = new HashMap<Long, List<BasicMessage>>();

    /** The list of current received messages. */
    private Map<Long, List<BasicMessage>> rxMessageMap = new HashMap<Long, List<BasicMessage>>();

    /** The list of current applications. */
    private List<String> applicationList = new ArrayList<String>();

    /** The collection of started applications. */
    private Collection<String> startedApplicationList = new ArrayList<String>();

    /** The collection of stopped applications. */
    private Collection<String> stoppedApplicationList = new ArrayList<String>();

    /** The simulation factory. */
    @Resource(name = "genericAdapter")
    private SimFactory genericFactory;

    /** The map of interfaces to simulation engines. */
    private Map<Long, SimulatorInterface> simulatorMap;

    /**
     * Creates a new <code>ExecutionRunner</code> instance.
     * 
     * @return A new <code>ExecutionRunner</code> instance.
     * @throws WebAppException If a new instance cannot be created.
     */
    public static ExecutionRunner createInstance() throws WebAppException {

        try {

            InitialContext context = new InitialContext();
            return (ExecutionRunner)context.lookup(String.format("java:module/%s", ExecutionRunner.class.getSimpleName()));
        }
        catch (NamingException exception) {

            String exceptionTitle = "Create Instance Failure";
            LoggerFactory.getLogger(ExecutionRunner.class).error(exceptionTitle, exception);
            throw new WebAppException(exceptionTitle, "A new execution runner could not be created.");
        }
    }

    /**
     * Starts a new execution for the specified composite.
     * 
     * @param userId The user ID.
     * @param composite The composite to execute.
     * @param executionName The string execution name to set.
     * @return The started execution.
     * @throws WebAppException If a new execution cannot be started.
     */
    public Execution start(Long userId, Composite composite, String executionName) throws WebAppException {

        execution = new Execution();
        execution.setName(executionName);
        execution.setMaximumSteps(composite.getMaximumSteps());
        execution.setStepSize(composite.getStepSize());
        execution.setStatus(ExecutionStatus.IN_PROGRESS);
        composite.getExecutions().add(execution);
        entityManager.flush();

        ComponentContainer components;

        try {

            Map<Long, ExecMetaData> metaDataMap = ExecutionBuilder.buildExecMetaData(composite);
            List<IDeviceData> deviceData = ExecutionBuilder.buildDeviceData(composite);
            NativeAppManager nativeApplicationManager = ExecutionBuilder.buildNativeApplicationManager(execution.getId(), deviceData);
            List<AppInitConfig> cellularConfigurations = ExecutionBuilder.buildCellularApplicationConfigurations(composite.getDeviceProfiles());
            List<AppInitConfig> dsrcConfigurations = ExecutionBuilder.buildDsrcApplicationConfigurations(composite.getDeviceProfiles());
            Map<String, String> waveParameterMap = ExecutionBuilder.buildWaveParameters();
            CellularConfig cellularConfig = ExecutionBuilder.buildCellularConfig(composite);
            List<CellTower> cellTowers = ExecutionBuilder.buildCellTowers(composite);
            ITopography topography = ExecutionBuilder.buildTopography(composite);
            simulatorMap = ExecutionBuilder.buildSimulatorMap(genericFactory, composite, execution.getId());

            components = new DefaultComponentCreator().build(simulatorMap, metaDataMap, deviceData, dsrcConfigurations, cellularConfigurations, cellularConfig, cellTowers, topography,
                    composite.getCommunicationsModel(), nativeApplicationManager, new ExecutionPersistenceLayer(execution.getId()), waveParameterMap);
        }
        catch (WebAppException exception) {

            stop();
            execution.setStatus(ExecutionStatus.ERROR);
            throw new WebAppException("Start Execution Failure", exception.getMessage(), exception.getStatus());
        }

        remoteManager = components.remoteAppManager;
        driver = new SingleIntersectionDriver(components);
        dataLayer = components.data;

        return execution;
    }

    /**
     * Stops the running execution.
     */
    public void stop() {

        if (simulatorMap != null) {

            isFinished = true;
            LoggerFactory.getLogger(ExecutionRunner.class).info("Execution {}: closing.", execution.getId());

            for (SimulatorInterface simulator : simulatorMap.values()) {

                try {

                    simulator.close();

                    LoggerFactory.getLogger(ExecutionRunner.class).info("Execution {}: closed.", execution.getId());
                }
                catch (RemoteException exception) {

                    LoggerFactory.getLogger(ExecutionRunner.class).error("Could not close connection to execution!", exception);
                }
            }
        }

    }

    /**
     * Advances the execution by the specified number of steps.
     * 
     * @param numSteps The integer number of steps to advance.
     * @return The running execution.
     */
    public Execution stepExecution(int numSteps) {

        try {

            execution = entityManager.merge(execution);
            List<ExecutionMessage> executionMessages = nextTimeStep(numSteps);

            execution.addExecutionMessages(executionMessages);
            execution.setStepNumber(Math.min(execution.getStepNumber() + numSteps, execution.getMaximumSteps()));

            // Update the execution status
            if (isFinished && !hasSimulatorError) {

                completeExecution();
            }
            else if (hasSimulatorError) {

                execution.setStatus(ExecutionStatus.ERROR);
                entityManager.flush();
                stop();
            }
            else {

                entityManager.flush();
            }

        }
        catch (SimDriverException e) {

            execution.setStatus(ExecutionStatus.ERROR);
            entityManager.flush();
            stop();
        }
        return execution;
    }

    /**
     * Completes the running execution.
     */
    @Remove
    private void completeExecution() {

        execution = entityManager.merge(execution);
        execution.setStatus(ExecutionStatus.COMPLETE);
        entityManager.flush();
        stop();
    }

    /**
     * Deletes the running execution.
     */
    @Remove
    public void deleteExecution() {

        execution = entityManager.merge(execution);
        entityManager.remove(execution);
        entityManager.flush();
        stop();
    }

    /**
     * Returns the detectors in the running execution.
     * 
     * @return A map of detectors in the running execution.
     */
    public Map<Long, List<IDetector>> getDetectors() {

        Map<Long, List<IDetector>> detectorMap = new HashMap<Long, List<IDetector>>();

        for (Entry<Long, InterRepInfoModel> modelEntry : modelMap.entrySet()) {

            InterRepInfoModel model = modelEntry.getValue();
            List<IDetector> detectors = new ArrayList<IDetector>();

            if (model != null) {

                Iterator<IDetector> detectorIterator = model.dmi.iterator();

                while (detectorIterator.hasNext()) {

                    detectors.add(detectorIterator.next());
                }
            }

            detectorMap.put(modelEntry.getKey(), detectors);
        }

        return detectorMap;
    }

    /**
     * Adds a new injection command to the specified simulation.
     * 
     * @param simulationId The long ID of the simulation.
     * @param laneId The integer ID of the target lane.
     * @param speed The double speed (m/s).
     */
    public void addInjectionCommand(Long simulationId, Integer laneId, double speed) {

        Vehicle vehicle = new Vehicle(dataLayer.nextInjectedVehicleId(), 0.0, 0.0, 0.0, 0.0, 0.0);
        vehicle.setLaneID(laneId);
        vehicle.setSpeed(speed);
        vehicle.setAcceleration(0.0);

        VehicleInjectionRequest injectionCommand = new VehicleInjectionRequest();
        injectionCommand.setIntersectionId(simulationId.intValue());
        injectionCommand.setInjectionTime(getSimulationTime());
        injectionCommand.setVehicle(vehicle);

        dataLayer.addVehicleInjectionRequest(CommandSource.USER, stepCount, injectionCommand, simulationId.intValue());
    }

    /**
     * Adds a new lane command to the specified simulation.
     * 
     * @param simulationId The long ID of the simulation.
     * @param vehicleId The integer ID of the vehicle.
     * @param command The integer command type.
     */
    public void addLaneCommand(Long simulationId, Integer vehicleId, Integer command) {

        VehicleLaneChangeCommand laneCommand = new VehicleLaneChangeCommand(vehicleId, command);
        dataLayer.addVehicleCommand(CommandSource.USER, stepCount, laneCommand, simulationId.intValue());
    }

    /**
     * Adds a new signal command to the specified simulation.
     * 
     * @param simulationId The long ID of the simulation.
     * @param command The integer command type.
     * @param time The double signal time (s).
     */
    public void addSignalCommand(Long simulationId, Integer command, Double time) {

        SignalCommand signalCommand = new SignalCommand(command, time);
        dataLayer.addSignalCommand(CommandSource.USER, stepCount, signalCommand, simulationId.intValue());
    }

    /**
     * Adds a new speed command to the specified simulation.
     * 
     * @param simulationId The long ID of the simulation.
     * @param vehicleId The integer ID of the vehicle.
     * @param command The integer command type.
     * @param speed The double speed (m/s).
     */
    public void addSpeedCommand(Long simulationId, Integer vehicleId, Integer command, Double speed) {

        VehicleSpeedCommand speedCommand = new VehicleSpeedCommand(vehicleId, command, speed);
        dataLayer.addVehicleCommand(CommandSource.USER, stepCount, speedCommand, simulationId.intValue());
    }

    /**
     * Returns the signal indications in the running execution.
     * 
     * @return A map of signal indications in the running execution.
     */
    public Map<Long, List<ISignalIndication>> getSignalIndications() {

        Map<Long, List<ISignalIndication>> signalMap = new HashMap<Long, List<ISignalIndication>>();

        for (Entry<Long, InterRepInfoModel> modelEntry : modelMap.entrySet()) {

            InterRepInfoModel model = modelEntry.getValue();
            List<ISignalIndication> signals = new ArrayList<ISignalIndication>();

            if (model != null) {

                Iterator<ISignalIndication> signalIterator = model.smi.iterator();

                while (signalIterator.hasNext()) {

                    signals.add(signalIterator.next());
                }
            }

            signalMap.put(modelEntry.getKey(), signals);
        }

        return signalMap;
    }

    /**
     * Returns the standalone devices in the running execution.
     * 
     * @return A list of standalone devices in the running execution.
     */
    public List<StandaloneDevice> getStandaloneDevices() {

        List<StandaloneDevice> standaloneDevices = new LinkedList<StandaloneDevice>();
        Iterable<? extends IDeviceData> activeDevices = dataLayer.getCurrentDevices(stepCount);

        for (IDeviceData idd : activeDevices) {

            StandaloneDevice standaloneDevice = null;

            if (idd instanceof RSEDeviceData) {

                RSEDeviceData rse = (RSEDeviceData)idd;
                standaloneDevice = new StandaloneDevice(idd.getDeviceRuleId(), idd.getMacAddress(), StandaloneDeviceType.RSE, rse.getX(), rse.getY(), rse.getZ());
            }
            else if (idd instanceof FixedCellDeviceData) {

                FixedCellDeviceData fcdd = (FixedCellDeviceData)idd;
                standaloneDevice = new StandaloneDevice(idd.getDeviceRuleId(), idd.getMacAddress(), StandaloneDeviceType.FIXED_CELL, fcdd.x, fcdd.y, fcdd.z);
            }

            if (standaloneDevice != null) {

                standaloneDevices.add(standaloneDevice);
            }
        }

        return standaloneDevices;
    }

    /**
     * Returns the on board devices for the specified vehicle.
     *
     * @param vehicleId The ID of the vehicle.
     * @return A list of all on board devices for the specified vehicle.
     * @throws WebAppException If the on board devices cannot be retrieved.
     */
    public List<OnBoardDevice> getOnBoardDevices(long vehicleId) throws WebAppException {

        IVehicle vehicle = null;
        String vehicleProperId = dataLayer.getProperVehicleId(vehicleId);

        for (InterRepInfoModel model : modelMap.values()) {

            if (model.vmi.getVehicle(vehicleProperId) != null) {

                vehicle = model.vmi.getVehicle(vehicleProperId);
                break;
            }
        }

        if (vehicle == null) {

            throw new WebAppException("Get On Board Devices Failure", String.format("No vehicle with ID \"%s\" could be found in the execution.", vehicleId));
        }

        List<OnBoardDevice> onBoardDevices = new ArrayList<OnBoardDevice>();
        Iterable<? extends IDeviceData> activeDevices = dataLayer.getCurrentDevices(stepCount);

        for (IDeviceData idd : activeDevices) {

            if (idd instanceof AbstractEmbeddedDeviceData) {

                AbstractEmbeddedDeviceData deviceData = (AbstractEmbeddedDeviceData)idd;

                if (deviceData.getVehicleId().equals(vehicleProperId)) {

                    OnBoardDeviceType deviceType = (deviceData instanceof OBUDeviceData) ? OnBoardDeviceType.OBU : OnBoardDeviceType.CELL;
                    onBoardDevices.add(new OnBoardDevice(deviceData.getDeviceRuleId(), deviceData.getMacAddress(), deviceType));
                }
            }
        }

        return onBoardDevices;
    }

    /**
     * Returns the vehicles in the running execution.
     * 
     * @return A map of vehicles in the running execution.
     * @throws WebAppException If the vehicles cannot be retrieved.
     */
    public Map<Long, List<org.etexascode.webapp.rest.model.Vehicle>> getVehicles() throws WebAppException {

        Map<String, List<OnBoardDevice>> onBoardDeviceMap = getOnBoardDevicesByVehicle();

        Map<Long, List<org.etexascode.webapp.rest.model.Vehicle>> vehicleMap = new HashMap<Long, List<org.etexascode.webapp.rest.model.Vehicle>>();

        for (Entry<Long, InterRepInfoModel> modelEntry : modelMap.entrySet()) {

            InterRepInfoModel model = modelEntry.getValue();
            List<org.etexascode.webapp.rest.model.Vehicle> vehicles = new ArrayList<org.etexascode.webapp.rest.model.Vehicle>();

            if (model != null) {

                Iterator<IVehicle> vehicleIterator = model.vmi.iterator();

                while (vehicleIterator.hasNext()) {

                    IVehicle iVehicle = vehicleIterator.next();

                    vehicles.add(new org.etexascode.webapp.rest.model.Vehicle(iVehicle, onBoardDeviceMap.get(iVehicle.getProperId())));
                }
            }

            vehicleMap.put(modelEntry.getKey(), vehicles);
        }

        return vehicleMap;
    }

    /**
     * Returns the on board devices by vehicle in the running execution.
     * 
     * @return A map of on board devices by vehicle in the running execution.
     */
    public Map<String, List<OnBoardDevice>> getOnBoardDevicesByVehicle() {

        Map<String, List<OnBoardDevice>> onBoardDeviceMap = new HashMap<String, List<OnBoardDevice>>();
        Iterable<? extends IDeviceData> activeDevices = dataLayer.getCurrentDevices(stepCount);

        for (IDeviceData idd : activeDevices) {

            if (idd instanceof AbstractEmbeddedDeviceData) {

                AbstractEmbeddedDeviceData deviceData = (AbstractEmbeddedDeviceData)idd;

                OnBoardDeviceType deviceType = (deviceData instanceof OBUDeviceData) ? OnBoardDeviceType.OBU : OnBoardDeviceType.CELL;
                OnBoardDevice device = new OnBoardDevice(deviceData.getDeviceRuleId(), deviceData.getMacAddress(), deviceType);

                if (onBoardDeviceMap.containsKey(deviceData.getVehicleId())) {

                    List<OnBoardDevice> devices = onBoardDeviceMap.get(deviceData.getVehicleId());
                    devices.add(device);
                }
                else {

                    List<OnBoardDevice> devices = new ArrayList<OnBoardDevice>();
                    devices.add(device);
                    onBoardDeviceMap.put(deviceData.getVehicleId(), devices);
                }
            }
        }
        return onBoardDeviceMap;
    }

    /**
     * Returns the current simulation time.
     * 
     * @return The double current simulation time.
     */
    public double getSimulationTime() {

        return stepCount * execution.getStepSize();
    }

    /**
     * Advances the simulation driver by specified number of steps.
     * 
     * @param steps The integer number of steps to advance.
     * @return The reported execution messages.
     */
    private List<ExecutionMessage> nextTimeStep(int steps) {

        this.stepCount += steps;
        List<ExecutionMessage> executionMessages = new ArrayList<ExecutionMessage>();
        if (!isFinished) {

            LoggerFactory.getLogger(ExecutionRunner.class).info("Execution {}: stepping {} times.", execution.getId(), Integer.toString(steps));

            isFinished = !driver.execSteps(steps);

            for (Entry<Long, SimulatorInterface> simulatorEntry : simulatorMap.entrySet()) {

                SimulatorInterface simulator = simulatorEntry.getValue();
                String uuid = String.format("exec%dsim%d", execution.getId(), simulatorEntry.getKey());

                try {

                    for (SimulatorMessage simulatorMessage : simulator.checkForErrorOutput(uuid)) {

                        hasSimulatorError = true;
                        ExecutionMessage executionMessage = new ExecutionMessage();
                        executionMessage.setSimulationId(simulatorEntry.getKey());
                        executionMessage.setType(ExecutionMessageType.ERROR);
                        executionMessage.setMessages(simulatorMessage.getMessage());
                        executionMessages.add(executionMessage);
                    }

                    for (SimulatorMessage simulatorMessage : simulator.checkForWarningOutput(uuid)) {

                        ExecutionMessage executionMessage = new ExecutionMessage();
                        executionMessage.setSimulationId(simulatorEntry.getKey());
                        executionMessage.setType(ExecutionMessageType.WARNING);
                        executionMessage.setMessages(simulatorMessage.getMessage());
                        executionMessages.add(executionMessage);
                    }
                }
                catch (RemoteException exception) {

                    LoggerFactory.getLogger(ExecutionRunner.class).error("Unable to connect to simulation.", exception);
                    throw new SimDriverException("Execution Error", "Unable to connect to simulation, shutting down the execution.");
                }
            }

            txMessageMap = dataLayer.getTxMessages(stepCount);
            rxMessageMap = dataLayer.getRxMessages(stepCount);

            // Add and remove app IDs to the UI.
            startedApplicationList = remoteManager.getStartedApps();
            stoppedApplicationList = remoteManager.getStoppedApps();
            applicationList.removeAll(stoppedApplicationList);
            applicationList.addAll(startedApplicationList);
        }

        // Put most recent InterRepInfoModel in BlockingQueue
        for (Long simulationId : simulatorMap.keySet()) {

            modelMap.put(simulationId, dataLayer.getInfoModel(0, simulationId.intValue()));
        }

        if (isFinished) {

            driver.shutdownNicely();
        }

        return executionMessages;
    }

    /**
     * Returns the list of messages.
     * 
     * @return The list of messages for the execution.
     * @throws WebAppException If the messages couldn't be retrieved.
     */
    public List<MessageData> getMessages() throws WebAppException {

        Map<Long, List<MessageData>> messageMap = new HashMap<Long, List<MessageData>>();
        List<MessageData> retMessages = new ArrayList<MessageData>();

        for (Long key : txMessageMap.keySet()) {

            messageMap.put(key, getMessagesByDevice(key));
        }

        for (Long key : rxMessageMap.keySet()) {

            if (!messageMap.containsKey(key)) {

                messageMap.put(key, getMessagesByDevice(key));
            }
        }

        for (List<MessageData> messages : messageMap.values()) {

            retMessages.addAll(messages);
        }

        return retMessages;
    }

    /**
     * Returns the list of messages for a device.
     * 
     * @param deviceMac The MAC address of the device.
     * @return The list of messages for a device.
     * @throws WebAppException If the messages couldn't be retrieved.
     */
    public List<MessageData> getMessagesByDevice(Long deviceMac) throws WebAppException {

        List<MessageData> retMessages = new ArrayList<MessageData>();
        retMessages.addAll(convertToMessageData(txMessageMap.get(deviceMac)));
        retMessages.addAll(convertToMessageData(rxMessageMap.get(deviceMac)));

        for (int i = 0; i < retMessages.size(); i++) {

            MessageData message = retMessages.get(i);

            // Adds a unique identifier
            message.setMessageId(i);
            message.setOwnerDeviceAddress(deviceMac);

            if (message.getOriginAddress() == deviceMac) {

                message.setType("Tx");
            }
            else {

                message.setType("Rx");
            }
        }

        return retMessages;
    }

    /**
     * Converts BasicMessages into MessageData.
     * 
     * @param messages The list of BasicMessage.
     * @return The list of MessageData.
     * @throws WebAppException If the messages couldn't be converted.
     */
    private List<MessageData> convertToMessageData(List<BasicMessage> messages) throws WebAppException {

        if (messages == null) {
            return new ArrayList<MessageData>();
        }

        List<MessageData> retMessages = new ArrayList<MessageData>();

        for (BasicMessage current : messages) {

            MessageData messageData = new MessageData();
            messageData.setOriginAddress(current.getOriginMACAddress());
            messageData.setDestinationAddress(current.getPeerMACAddress());

            Object data = current.getData();

            if (data instanceof byte[]) {

                try {

                    messageData.setMessage(new String((byte[])data, "UTF-8"));
                }
                catch (UnsupportedEncodingException e) {

                    throw new WebAppException("Messages", "Unable to encode the message with UTF-8 encoding");
                }
            }
            else {

                messageData.setMessage(data);
            }

            if (current instanceof IFormattedMessage) {

                Object formattedData = ((IFormattedMessage)current).getFormattedData();
                messageData.setFormattedMessage(((IFormattedMessage)current).getFormattedData());
                messageData.setMessageType(getMessageType(formattedData));
            }
            else {

                messageData.setMessageType(getMessageType(data));
            }

            if (current instanceof DSRCMessage) {

                messageData.setChannel(((DSRCMessage)current).getChannel());
            }

            retMessages.add(messageData);
        }

        return retMessages;
    }

    /**
     * Returns the message type for the specified message data.
     * 
     * @param data The message data.
     * @return The string message type for the specified message data.
     */
    private static String getMessageType(Object data) {

        String messageType = null;

        if (data instanceof BasicSafetyMessage) {

            messageType = "BSM";
        }
        else if (data instanceof BasicSafetyMessageVerbose) {

            messageType = "BSMV";
        }
        else if (data instanceof SPAT) {

            messageType = "SPAT";
        }
        else if (data instanceof MapData) {

            messageType = "Map Data";
        }
        else if (data instanceof SimData) {

            messageType = "SimData";
        }
        else if (data instanceof DetectorData) {

            messageType = "Detector Data";
        }
        else if (data instanceof CommonSafetyRequest) {

            messageType = "CSR";
        }
        else if (data instanceof BasicCellMessage) {

            messageType = "Cell Message";
        }
        else if (data instanceof byte[]) {

            messageType = "Raw Bytes";
        }
        else {

            messageType = "Unknown";
        }

        return messageType;
    }
}
