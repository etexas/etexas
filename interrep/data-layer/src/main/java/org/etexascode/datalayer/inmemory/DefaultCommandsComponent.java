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
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.datalayer.interfaces.ICommandsComponent;
import org.etexascode.interrep.datamodel.Command;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;

/**
 * The default commands component.
 * 
 * @author emyers
 */
public class DefaultCommandsComponent implements ICommandsComponent {

    /** The map of signal commands. */
    private Map<Integer, List<SignalCommand>> signalCommandsMap;

    /** The map of vehicle commands. */
    private Map<Integer, List<VehicleCommand>> vehicleCommandsMap;

    /** The map of vehicle injection requests. */
    private Map<Integer, List<VehicleInjectionRequest>> injectionRequestsMap;

    /**
     * Creates a new <code>DefaultCommandsComponent</code> instance.
     */
    public DefaultCommandsComponent() {

        signalCommandsMap = new HashMap<Integer, List<SignalCommand>>();
        vehicleCommandsMap = new HashMap<Integer, List<VehicleCommand>>();
        injectionRequestsMap = new HashMap<Integer, List<VehicleInjectionRequest>>();
    }

    @Override
    public void putSignalCommand(int stepNum, int interRepId, SignalCommand command) {

        putSignalCommands(stepNum, interRepId, Arrays.asList(command));
    }

    @Override
    public void putSignalCommands(int stepNum, int interRepId, List<SignalCommand> commands) {

        List<SignalCommand> signalCommands = signalCommandsMap.get(interRepId);

        if (signalCommands == null) {

            signalCommands = new ArrayList<SignalCommand>();
            signalCommandsMap.put(interRepId, signalCommands);
        }

        signalCommands.addAll(commands);
    }

    @Override
    public void putVehicleCommand(int stepNum, int interRepId, VehicleCommand command) {

        putVehicleCommands(stepNum, interRepId, Arrays.asList(command));
    }

    @Override
    public void putVehicleCommands(int stepNum, int interRepId, List<VehicleCommand> commands) {

        List<VehicleCommand> vehicleCommands = vehicleCommandsMap.get(interRepId);

        if (vehicleCommands == null) {

            vehicleCommands = new ArrayList<VehicleCommand>();
            vehicleCommandsMap.put(interRepId, vehicleCommands);
        }

        vehicleCommands.addAll(commands);
    }

    @Override
    public void putVehicleInjection(int stepNum, int interRepId, VehicleInjectionRequest injection) {

        putVehicleInjections(stepNum, interRepId, Arrays.asList(injection));
    }

    @Override
    public void putVehicleInjections(int stepNum, int interRepId, List<VehicleInjectionRequest> injections) {

        List<VehicleInjectionRequest> injectionRequests = injectionRequestsMap.get(interRepId);

        if (injectionRequests == null) {

            injectionRequests = new ArrayList<VehicleInjectionRequest>();
            injectionRequestsMap.put(interRepId, injectionRequests);
        }

        injectionRequests.addAll(injections);
    }

    @Override
    public List<SignalCommand> getSignalCommands(int stepNum, int interRepId) {

        List<SignalCommand> signalCommands = signalCommandsMap.put(interRepId, new ArrayList<SignalCommand>());
        return (signalCommands != null) ? signalCommands : new ArrayList<SignalCommand>();
    }

    @Override
    public List<VehicleCommand> getVehicleCommands(int stepNum, int interRepId) {

        List<VehicleCommand> vehicleCommands = vehicleCommandsMap.put(interRepId, new ArrayList<VehicleCommand>());
        return (vehicleCommands != null) ? vehicleCommands : new ArrayList<VehicleCommand>();
    }

    @Override
    public List<VehicleInjectionRequest> getVehicleInjections(int stepNum, int interRepId) {

        List<VehicleInjectionRequest> injectionRequests = injectionRequestsMap.put(interRepId, new ArrayList<VehicleInjectionRequest>());
        return (injectionRequests != null) ? injectionRequests : new ArrayList<VehicleInjectionRequest>();
    }

    @Override
    public Collection<? extends Command> peekSignalCommands(int stepNum, int interRepId) {

        List<SignalCommand> signalCommands = signalCommandsMap.get(interRepId);
        return Collections.unmodifiableList((signalCommands != null) ? signalCommands : new ArrayList<SignalCommand>());
    }

    @Override
    public Collection<? extends Command> peekVehicleCommands(int stepNum, int interRepId) {

        List<VehicleCommand> vehicleCommands = vehicleCommandsMap.get(interRepId);
        return Collections.unmodifiableList((vehicleCommands != null) ? vehicleCommands : new ArrayList<VehicleCommand>());
    }

    @Override
    public Collection<? extends Command> peekVehicleInjections(int stepNum, int interRepId) {

        List<VehicleInjectionRequest> injectionRequests = injectionRequestsMap.get(interRepId);
        return Collections.unmodifiableList((injectionRequests != null) ? injectionRequests : new ArrayList<VehicleInjectionRequest>());
    }
}
