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

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.etexascode.datalayer.interfaces.ICommandsComponent;
import org.etexascode.datalayer.interfaces.IDevicesComponent;
import org.etexascode.datalayer.interfaces.IIntersectionModelsComponent;
import org.etexascode.datalayer.interfaces.IMessageComponent;
import org.etexascode.datalayer.interfaces.ITemporalComponent;
import org.etexascode.datalayer.interfaces.IVehicleIdComponent;
import org.etexascode.datalayer.interfaces.IWebAppDataLayer;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.interrep.datamodel.Command;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.persistencelayer.IPersistenceLayer;

/**
 * The default web application data layer.
 * 
 * @author ablatt
 * @author ttevendale
 * @author emyers
 */
public class DefaultWebAppDataLayer implements IWebAppDataLayer {

    /** The commands component. */
    private ICommandsComponent commandsComponent;

    /** The devices component. */
    private IDevicesComponent devicesComponent;

    /** The intersection models component. */
    private IIntersectionModelsComponent modelsComponent;

    /** The message component. */
    private IMessageComponent messageComponent;

    /** The persistence layer. */
    private IPersistenceLayer persistenceLayer;

    /** The temporal component. */
    private ITemporalComponent temporalComponent;

    /** The vehicle ID component */
    private IVehicleIdComponent vehicleIdComponent;

    /**
     * Creates a new <code>DefaultWebAppDataLayer</code> with the specified components.
     * 
     * @param commandsComponent The commands component for this data layer.
     * @param devicesComponent The devices component for this data layer.
     * @param modelsComponent The intersection models component for this data layer.
     * @param messageComponent The message component for this data layer.
     * @param persistenceLayer The persistence layer for this data layer.
     * @param temporalComponent The temporal component for this data layer.
     * @param vehicleIdComponent The vehicle ID component for this data layer.
     */
    public DefaultWebAppDataLayer(ICommandsComponent commandsComponent, IDevicesComponent devicesComponent, IIntersectionModelsComponent modelsComponent, IMessageComponent messageComponent,
            IPersistenceLayer persistenceLayer, ITemporalComponent temporalComponent, IVehicleIdComponent vehicleIdComponent) {

        this.commandsComponent = commandsComponent;
        this.devicesComponent = devicesComponent;
        this.modelsComponent = modelsComponent;
        this.messageComponent = messageComponent;
        this.persistenceLayer = persistenceLayer;
        this.temporalComponent = temporalComponent;
        this.vehicleIdComponent = vehicleIdComponent;
    }

    @Override
    public Iterable<? extends IVehicle> getAllVehiclesForTable(int stepNum, int interRepId) {

        return modelsComponent.getVehicles(stepNum, interRepId);
    }

    @Override
    public Iterable<? extends IDetector> getAllDetectors(int stepNum, int interRepId) {

        return modelsComponent.getDetectors(stepNum, interRepId);
    }

    @Override
    public Iterable<? extends IDeviceData> getCurrentDevices(int stepNum) {

        return devicesComponent.getActiveDevices(stepNum);
    }

    @Override
    public Map<Long, List<BasicMessage>> getTxMessages(int stepNum) {

        return messageComponent.getTxMessages(stepNum);
    }

    @Override
    public Map<Long, List<BasicMessage>> getRxMessages(int stepNum) {

        return messageComponent.getIndicationsForDevicesByTimeStep(stepNum);
    }

    @Override
    public ILane getLaneFromVehicle(int stepNum, IVehicle vi) {

        return modelsComponent.getLaneInfoByVehicle(stepNum, vi);
    }

    @Override
    public List<? extends ISignalIndication> getSignalIndication(int stepNum, ILane li) {

        return modelsComponent.getSignalsByLane(stepNum, li);
    }

    @Override
    public InterRepInfoModel getInfoModel(int stepNum, int interRepId) {

        return modelsComponent.getInfoModel(stepNum, interRepId);
    }

    @Override
    public void addSignalCommand(String source, int stepNum, SignalCommand command, int interRepId) {

        commandsComponent.putSignalCommand(stepNum, interRepId, command);
        persistenceLayer.persistSignalCommands(source, interRepId, temporalComponent.getSimTime(stepNum), Arrays.asList(command));
    }

    @Override
    public void addVehicleCommand(String source, int stepNum, VehicleCommand command, int interRepId) {

        commandsComponent.putVehicleCommand(stepNum, interRepId, command);
        persistenceLayer.persistVehicleCommands(source, interRepId, temporalComponent.getSimTime(stepNum), Arrays.asList(command));
    }

    @Override
    public void addVehicleInjectionRequest(String source, int stepNum, VehicleInjectionRequest request, int interRepId) {

        commandsComponent.putVehicleInjection(stepNum, interRepId, request);
        persistenceLayer.persistVehicleInjectionRequests(source, interRepId, temporalComponent.getSimTime(stepNum), Arrays.asList(request));
    }

    @Override
    public Collection<? extends Command> peekSignalCommands(int stepNum, int interRepId) {

        return commandsComponent.peekSignalCommands(stepNum, interRepId);
    }

    @Override
    public Collection<? extends Command> peekVehicleCommands(int stepNum, int interRepId) {

        return commandsComponent.peekVehicleCommands(stepNum, interRepId);
    }

    @Override
    public Collection<? extends Command> peekVehicleInjections(int stepNum, int interRepId) {

        return commandsComponent.peekVehicleInjections(stepNum, interRepId);
    }

    @Override
    public String getProperVehicleId(long globalId) {

        return vehicleIdComponent.getProperVehicleId(globalId);
    }

    @Override
    public int nextInjectedVehicleId() {

        return vehicleIdComponent.nextInjectedVehicleId();
    }
}
