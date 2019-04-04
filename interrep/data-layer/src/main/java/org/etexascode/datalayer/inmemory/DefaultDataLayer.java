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
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import org.etexascode.appslayerdata.AppLayerInput;
import org.etexascode.appslayerdata.AppLayerOutput;
import org.etexascode.appslayerdata.AppShutdownLayerInput;
import org.etexascode.appslayerdata.RemoteProxyApp;
import org.etexascode.datalayer.IDataLayer;
import org.etexascode.datalayer.interfaces.IAppShutdownDataLayer;
import org.etexascode.datalayer.interfaces.IAppsDataLayer;
import org.etexascode.datalayer.interfaces.ICommandsComponent;
import org.etexascode.datalayer.interfaces.IDevicesComponent;
import org.etexascode.datalayer.interfaces.IInterRepCoordinationDataLayer;
import org.etexascode.datalayer.interfaces.IIntersectionModelsComponent;
import org.etexascode.datalayer.interfaces.IMacManagerComponent;
import org.etexascode.datalayer.interfaces.IMessageComponent;
import org.etexascode.datalayer.interfaces.IRestDataLayer;
import org.etexascode.datalayer.interfaces.ITemporalComponent;
import org.etexascode.datalayer.interfaces.IVehicleIdComponent;
import org.etexascode.datalayer.interfaces.IVehicleLocationDataLayer;
import org.etexascode.datalayer.interfaces.IWaveSimDataLayer;
import org.etexascode.datalayer.interfaces.IWebAppDataLayer;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DualIntIdentifier;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.devicedata.LogData;
import org.etexascode.interrep.datamodel.Command;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.persistencelayer.IPersistenceLayer;
import org.etexascode.wavesim.Rx;
import org.etexascode.wavesim.Tx;

/**
 * The default data layer.
 * 
 * @author ablatt
 * @author ttevendale
 * @author emyers
 */
public class DefaultDataLayer implements IDataLayer {

    /** The random seed generator. */
    private static final Random SEED_GENERATOR = new Random();

    /** The applications data layer. */
    private IAppsDataLayer appsDataLayer;

    /** The application shutdown data layer. */
    private IAppShutdownDataLayer appShutdownDataLayer;

    /** The intersection coordination data layer. */
    private IInterRepCoordinationDataLayer interRepCoordinationDataLayer;

    /** The REST data layer. */
    private IRestDataLayer restDataLayer;

    /** The temporal component. */
    private ITemporalComponent temporalComponent;

    /** The vehicle location data layer. */
    private IVehicleLocationDataLayer vehicleLocationDataLayer;

    /** The WAVE simulation data layer. */
    private IWaveSimDataLayer waveSimDataLayer;

    /** The web application data layer. */
    private IWebAppDataLayer webAppDataLayer;

    /**
     * Creates a new <code>DefaultDataLayer</code> instance.
     * 
     * @param devices The list of devices for this data layer.
     * @param persistenceLayer The persistence layer for this data layer.
     * @param seedNumber The integer seed number for this data layer.
     */
    public DefaultDataLayer(List<IDeviceData> devices, IPersistenceLayer persistenceLayer, int seedNumber) {

        DefaultDataLayer.SEED_GENERATOR.setSeed(seedNumber);
        IMacManagerComponent macManagerComponent = new DefaultMacManagerComponent(DefaultDataLayer.SEED_GENERATOR.nextInt(), devices);

        for (IDeviceData device : devices) {

            device.setMacAddress(macManagerComponent.getMac(0, device.getProperId()));
        }

        ICommandsComponent commandsComponent = new DefaultCommandsComponent();
        IDevicesComponent devicesComponent = new DefaultDevicesComponent(devices);
        IIntersectionModelsComponent modelsComponent = new DefaultIntersectionModelsComponent();
        IMessageComponent messageComponent = new DefaultMessageComponent();
        temporalComponent = new DefaultTemporalComponent();
        IVehicleIdComponent vehicleIdComponent = new DefaultVehicleIdComponent();

        appsDataLayer = new DefaultAppsDataLayer(commandsComponent, devicesComponent, modelsComponent, macManagerComponent, messageComponent, persistenceLayer, temporalComponent);
        appShutdownDataLayer = new DefaultAppShutdownDataLayer(devicesComponent, macManagerComponent, persistenceLayer);
        interRepCoordinationDataLayer = new DefaultInterRepCoordinationDataLayer(commandsComponent, modelsComponent, temporalComponent);
        restDataLayer = new DefaultRestDataLayer(devicesComponent, temporalComponent);
        vehicleLocationDataLayer = new DefaultVehicleLocationDataLayer(commandsComponent, devicesComponent, modelsComponent, macManagerComponent, persistenceLayer, temporalComponent, seedNumber,
                vehicleIdComponent);
        waveSimDataLayer = new DefaultWaveSimDataLayer(messageComponent, temporalComponent);
        webAppDataLayer = new DefaultWebAppDataLayer(commandsComponent, devicesComponent, modelsComponent, messageComponent, persistenceLayer, temporalComponent, vehicleIdComponent);
    }

    @Override
    public long addVehicleId(IVehicle vehicle) {

        return vehicleLocationDataLayer.addVehicleId(vehicle);
    }

    @Override
    public void addVehicleInjectionRequest(String source, int stepNum, VehicleInjectionRequest request, int interRepId) {

        vehicleLocationDataLayer.addVehicleInjectionRequest(source, stepNum, request, interRepId);
    }

    @Override
    public Long getGlobalVehicleId(String properId) {

        return vehicleLocationDataLayer.getGlobalVehicleId(properId);
    }

    @Override
    public Set<String> getInjectedVehicles(int stepNum, int interRepId) {

        return vehicleLocationDataLayer.getInjectedVehicles(stepNum, interRepId);
    }

    @Override
    public String getProperVehicleId(long globalId) {

        return vehicleLocationDataLayer.getProperVehicleId(globalId);
    }

    @Override
    public int getRandomSeed(int stepNum, int interRepId) {

        return vehicleLocationDataLayer.getRandomSeed(stepNum, interRepId);
    }

    @Override
    public IVehicleManager getVehicleManagerInfo(int stepNum, int interRepId) {

        return vehicleLocationDataLayer.getVehicleManagerInfo(stepNum, interRepId);
    }

    @Override
    public boolean isStalledVehicle(IDable vehicle) {

        return vehicleLocationDataLayer.isStalledVehicle(vehicle);
    }

    @Override
    public int nextInjectedVehicleId() {

        return vehicleLocationDataLayer.nextInjectedVehicleId();
    }

    @Override
    public void putAppDeviceData(int stepNum, List<? extends IDeviceData> deviceData) {

        vehicleLocationDataLayer.putAppDeviceData(stepNum, deviceData);
    }

    @Override
    public void putLoggedOutVehicles(int stepNum, Map<DualIntIdentifier, List<IVehicle>> logoutVehicles) {

        vehicleLocationDataLayer.putLoggedOutVehicles(stepNum, logoutVehicles);
    }

    @Override
    public void putRandomSeed(int stepNum, int interRepId, int randomSeed) {

        vehicleLocationDataLayer.putRandomSeed(stepNum, interRepId, randomSeed);
    }

    @Override
    public void putReturnedVehicle(IDable vehicle) {

        vehicleLocationDataLayer.putReturnedVehicle(vehicle);
    }

    @Override
    public void putStalledVehicle(IDable oldVehicle, IDable newVehicle) {

        vehicleLocationDataLayer.putStalledVehicle(oldVehicle, newVehicle);
    }

    @Override
    public double getSimTime(int stepNum) {

        return temporalComponent.getSimTime(stepNum);
    }

    @Override
    public void putInterRepModel(int stepNum, int interRepId, InterRepInfoModel interRep) {

        interRepCoordinationDataLayer.putInterRepModel(stepNum, interRepId, interRep);
    }

    @Override
    public List<SignalCommand> getSignalCommands(int stepNum, int interRepId) {

        return interRepCoordinationDataLayer.getSignalCommands(stepNum, interRepId);
    }

    @Override
    public List<VehicleCommand> getVehicleCommands(int stepNum, int interRepId) {

        return interRepCoordinationDataLayer.getVehicleCommands(stepNum, interRepId);
    }

    @Override
    public List<VehicleInjectionRequest> getVehicleInjectionRequests(int stepNum, int interRepId) {

        return interRepCoordinationDataLayer.getVehicleInjectionRequests(stepNum, interRepId);
    }

    @Override
    public List<AppLayerInput> getAppData(int stepNum) {

        return appsDataLayer.getAppData(stepNum);
    }

    @Override
    public void putAppOutputs(int stepNum, List<AppLayerOutput> appOutputs) {

        appsDataLayer.putAppOutputs(stepNum, appOutputs);
    }

    @Override
    public Iterable<Tx> getTxs(int stepNum, Polygon segment) {

        return waveSimDataLayer.getTxs(stepNum, segment);
    }

    @Override
    public void putMessages(int stepNum, Collection<Rx> nodes) {

        waveSimDataLayer.putMessages(stepNum, nodes);
    }

    @Override
    public double getStepSize() {

        return temporalComponent.getStepSize();
    }

    /**
     * Sets the step size for this data layer.
     * 
     * @param stepSize The double step size to set.
     */
    public void setStepSize(double stepSize) {

        temporalComponent.setStepSize(stepSize);
    }

    @Override
    public void putAppLogs(int stepNum, List<LogData> logs) {

        appShutdownDataLayer.putAppLogs(stepNum, logs);
    }

    @Override
    public Iterable<AppShutdownLayerInput> getAppsToShutdown(int stepNum) {

        return appShutdownDataLayer.getAppsToShutdown(stepNum);
    }

    @Override
    public Iterable<AppShutdownLayerInput> getAllAppsForShutdown() {

        return appShutdownDataLayer.getAllAppsForShutdown();
    }

    @Override
    public Iterable<? extends IVehicle> getAllVehiclesForTable(int stepNum, int interRepId) {

        return webAppDataLayer.getAllVehiclesForTable(stepNum, interRepId);
    }

    @Override
    public Iterable<? extends IDetector> getAllDetectors(int stepNum, int interRepId) {

        return webAppDataLayer.getAllDetectors(stepNum, interRepId);
    }

    @Override
    public Iterable<? extends IDeviceData> getCurrentDevices(int stepNum) {

        return webAppDataLayer.getCurrentDevices(stepNum);
    }

    @Override
    public ILane getLaneFromVehicle(int stepNum, IVehicle vi) {

        return webAppDataLayer.getLaneFromVehicle(stepNum, vi);
    }

    @Override
    public List<? extends ISignalIndication> getSignalIndication(int stepNum, ILane li) {

        return webAppDataLayer.getSignalIndication(stepNum, li);
    }

    @Override
    public InterRepInfoModel getInfoModel(int stepNum, int interRepId) {

        return webAppDataLayer.getInfoModel(stepNum, interRepId);
    }

    @Override
    public void addSignalCommand(String source, int stepNum, SignalCommand command, int interRepId) {

        webAppDataLayer.addSignalCommand(source, stepNum, command, interRepId);
    }

    @Override
    public void addVehicleCommand(String source, int stepNum, VehicleCommand command, int interRepId) {

        webAppDataLayer.addVehicleCommand(source, stepNum, command, interRepId);
    }

    @Override
    public Collection<? extends Command> peekSignalCommands(int stepNum, int interRepId) {

        return webAppDataLayer.peekSignalCommands(stepNum, interRepId);
    }

    @Override
    public Collection<? extends Command> peekVehicleCommands(int stepNum, int interRepId) {

        return webAppDataLayer.peekVehicleCommands(stepNum, interRepId);
    }

    @Override
    public Collection<? extends Command> peekVehicleInjections(int stepNum, int interRepId) {

        return webAppDataLayer.peekVehicleInjections(stepNum, interRepId);
    }

    @Override
    public Map<Long, List<BasicMessage>> getRxMessages(int stepNum) {

        return webAppDataLayer.getRxMessages(stepNum);
    }

    @Override
    public Map<Long, List<BasicMessage>> getTxMessages(int stepNum) {

        return webAppDataLayer.getTxMessages(stepNum);
    }

    @Override
    public Iterable<IDeviceData> getStartedAppDevices(int stepNum) {

        return restDataLayer.getStartedAppDevices(stepNum);
    }

    @Override
    public List<RemoteProxyApp<?>> getStartedRemoteApps(int timestep) {

        return restDataLayer.getStartedRemoteApps(timestep);
    }

    @Override
    public Iterable<IDeviceData> getStoppedAppDevices(int stepNum) {

        return restDataLayer.getStoppedAppDevices(stepNum);
    }

    @Override
    public List<RemoteProxyApp<?>> getStoppedRemoteApps(int timestep) {

        return restDataLayer.getStoppedRemoteApps(timestep);
    }

    // TODO emyers - is this method really necessary?

    @Override
    public void shutdown() {

        appsDataLayer = null;
        appShutdownDataLayer = null;
        interRepCoordinationDataLayer = null;
        restDataLayer = null;
        vehicleLocationDataLayer = null;
        waveSimDataLayer = null;
        webAppDataLayer = null;
    }
}
