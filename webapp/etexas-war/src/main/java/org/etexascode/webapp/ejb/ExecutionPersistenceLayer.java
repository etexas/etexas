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
package org.etexascode.webapp.ejb;

import java.util.ArrayList;
import java.util.List;

import javax.naming.InitialContext;
import javax.naming.NamingException;

import org.etexascode.devicedata.LogData;
import org.etexascode.interrep.datamodel.Command;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.persistencelayer.IPersistenceLayer;
import org.etexascode.webapp.datamodel.ApplicationLog;

/**
 * The persistence layer for a running execution.
 * 
 * @author janway
 * @author emyers
 */
public class ExecutionPersistenceLayer implements IPersistenceLayer {

    /** The ID of the execution. */
    private Long executionId;

    /** The name of the EJB to persist execution commands. */
    private static final String EJB_NAME = String.format("java:module/%s", ExecutionManager.class.getSimpleName());

    /**
     * Creates a new <code>ExecutionPersistenceLayer</code> instance.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     */
    ExecutionPersistenceLayer(Long executionId) {

        this.executionId = executionId;
    }

    @Override
    public void persistAppLogs(List<LogData> logs) {

        ExecutionManager executionManager;

        try {

            executionManager = (ExecutionManager)new InitialContext().lookup(ExecutionPersistenceLayer.EJB_NAME);
        }
        catch (NamingException exception) {

            throw new RuntimeException("The EJB to persist execution application logs could not be found.");
        }

        List<ApplicationLog> persistedApplicationLogs = new ArrayList<ApplicationLog>();

        for (LogData log : logs) {

            ApplicationLog persistedApplicationLog = new ApplicationLog();
            persistedApplicationLog.setExecutionId(executionId);
            persistedApplicationLog.setDeviceId(log.getDeviceId());
            persistedApplicationLog.setApplicationName(log.getAppName());
            persistedApplicationLog.setSimulationTime(log.getSimTime().doubleValue());
            persistedApplicationLog.setApplicationKey(log.getKey());
            persistedApplicationLog.setApplicationMessage(log.getMessage());
            persistedApplicationLogs.add(persistedApplicationLog);
        }

        executionManager.batch(persistedApplicationLogs);
    }

    @Override
    public void shutdown() {}

    @Override
    public void persistVehicleCommands(String source, int intersectionId, double submittedTime, List<VehicleCommand> commands) {

        persistCommands(source, intersectionId, submittedTime, commands);
    }

    @Override
    public void persistSignalCommands(String source, int intersectionId, double submittedTime, List<SignalCommand> commands) {

        persistCommands(source, intersectionId, submittedTime, commands);
    }

    @Override
    public void persistVehicleInjectionRequests(String source, int intersectionId, double submittedTime, List<VehicleInjectionRequest> requests) {

        persistCommands(source, intersectionId, submittedTime, requests);
    }

    /**
     * Persists a list of execution commands.
     * 
     * @param source The source of the commands.
     * @param submittedTime The double submission time (s).
     * @param commands The list of commands to persist.
     */
    private void persistCommands(String source, int intersectionId, double submittedTime, List<? extends Command> commands) {

        ExecutionManager executionManager;

        try {

            executionManager = (ExecutionManager)new InitialContext().lookup(ExecutionPersistenceLayer.EJB_NAME);
        }
        catch (NamingException exception) {

            throw new RuntimeException("The EJB to persist execution commands could not be found.");
        }

        List<org.etexascode.webapp.datamodel.Command> persistedCommands = new ArrayList<org.etexascode.webapp.datamodel.Command>();

        for (Command command : commands) {

            org.etexascode.webapp.datamodel.Command persistedCommand = new org.etexascode.webapp.datamodel.Command();
            persistedCommand.setExecutionId(executionId);
            persistedCommand.setIntersectionId(intersectionId);
            persistedCommand.setSource(source);
            persistedCommand.setDescription(command.getDescription());
            persistedCommand.setSubmissionTime(submittedTime);
            persistedCommands.add(persistedCommand);
        }

        executionManager.batch(persistedCommands);
    }
}
