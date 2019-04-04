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
package org.etexascode.persistencelayer.interfaces;

import java.util.List;

import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;

/**
 * Persistence layer for commands and vehicle injections.
 * 
 * @author janway
 */
public interface ICommandsPersistenceLayer {

    /**
     * Writes vehicle commands out to the database.
     * 
     * @param source The source of the commands.
     * @param intersectionId The ID of the intersection for the commands.
     * @param submittedTime The time the commands were submitted.
     * @param commands The commands.
     */
    public void persistVehicleCommands(String source, int intersectionId, double submittedTime, List<VehicleCommand> commands);

    /**
     * Writes signal commands out to the database.
     * 
     * @param source The source of the commands.
     * @param intersectionId The ID of the intersection for the commands.
     * @param submittedTime The time the commands were submitted.
     * @param commands The commands.
     */
    public void persistSignalCommands(String source, int intersectionId, double submittedTime, List<SignalCommand> commands);

    /**
     * Writes vehicle injection requests out to the database.
     * 
     * @param source The source of the requests.
     * @param intersectionId The ID of the intersection for the requests.
     * @param submittedTime The time the requests were submitted.
     * @param requests The requests.
     */
    public void persistVehicleInjectionRequests(String source, int intersectionId, double submittedTime, List<VehicleInjectionRequest> requests);
}
