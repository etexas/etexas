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

import java.util.List;

import org.etexascode.datalayer.interfaces.ICommandsComponent;
import org.etexascode.datalayer.interfaces.IInterRepCoordinationDataLayer;
import org.etexascode.datalayer.interfaces.IIntersectionModelsComponent;
import org.etexascode.datalayer.interfaces.ITemporalComponent;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;

/**
 * The default intersection coordination data layer.
 * 
 * @author ablatt
 * @author ttevendale
 * @author emyers
 */
public class DefaultInterRepCoordinationDataLayer implements IInterRepCoordinationDataLayer {

    /** The commands component. */
    private ICommandsComponent commandsComponent;

    /** The models component. */
    private IIntersectionModelsComponent modelsComponent;

    /** The temporal component. */
    private ITemporalComponent temporalComponent;

    /**
     * Creates a new <code>DefaultInterRepCoordinationDataLayer</code> with the specified
     * components.
     * 
     * @param commandsComponent The commands component for this data layer.
     * @param modelsComponent The models component for this data layer.
     * @param temporalComponent The temporal component for this data layer.
     */
    public DefaultInterRepCoordinationDataLayer(ICommandsComponent commandsComponent, IIntersectionModelsComponent modelsComponent, ITemporalComponent temporalComponent) {

        this.commandsComponent = commandsComponent;
        this.modelsComponent = modelsComponent;
        this.temporalComponent = temporalComponent;
    }

    @Override
    public void putInterRepModel(int stepNum, int interRepId, InterRepInfoModel interRep) {

        modelsComponent.putInterRepInfoModel(stepNum, interRepId, interRep);
    }

    @Override
    public List<SignalCommand> getSignalCommands(int stepNum, int interRepId) {

        return commandsComponent.getSignalCommands(stepNum, interRepId);
    }

    @Override
    public List<VehicleCommand> getVehicleCommands(int stepNum, int interRepId) {

        return commandsComponent.getVehicleCommands(stepNum, interRepId);
    }

    @Override
    public List<VehicleInjectionRequest> getVehicleInjectionRequests(int stepNum, int interRepId) {

        return commandsComponent.getVehicleInjections(stepNum, interRepId);
    }

    @Override
    public double getSimTime(int stepNum) {

        return temporalComponent.getSimTime(stepNum);
    }
}
