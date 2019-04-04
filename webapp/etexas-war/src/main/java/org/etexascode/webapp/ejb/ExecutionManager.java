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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.ws.rs.core.Response;

import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.webapp.cdi.ExecutionMap;
import org.etexascode.webapp.datamodel.AbstractEntity;
import org.etexascode.webapp.datamodel.ApplicationLog;
import org.etexascode.webapp.datamodel.Command;
import org.etexascode.webapp.datamodel.Composite;
import org.etexascode.webapp.datamodel.Execution;
import org.etexascode.webapp.datamodel.ExecutionMessage;
import org.etexascode.webapp.datamodel.ExecutionStatus;
import org.etexascode.webapp.datamodel.MessageData;
import org.etexascode.webapp.datamodel.Simulation;
import org.etexascode.webapp.exception.WebAppException;
import org.etexascode.webapp.rest.model.OnBoardDevice;
import org.etexascode.webapp.rest.model.StandaloneDevice;
import org.etexascode.webapp.rest.model.Vehicle;

/**
 * The EJB to manage execution database transactions.
 * 
 * @author bbadillo
 * @author ablatt
 * @author jrutherford
 * @author dranker
 * @author ttevendale
 * @author emyers
 */
@Stateless
public class ExecutionManager {

    /** The composite transaction manager. */
    @Inject
    private CompositeManager compositeManager;

    /** The database entity manager. */
    @PersistenceContext(unitName = "etexas-pu")
    private EntityManager entityManager;

    /** The map of running executions. */
    @Inject
    private ExecutionMap executionMap;

    /** The number of records in each batch. */
    private int batchSize;

    /**
     * Returns the executions for the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @return A list of executions for the specified composite.
     * @throws WebAppException If the executions cannot be retrieved.
     */
    public List<Execution> getExecutions(Long userId, Long compositeId) throws WebAppException {

        try {

            compositeManager.getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Executions Failure", exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.EXECUTIONS_FROM_USER_ID_COMPOSITE_ID, Execution.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .getResultList();
    }

    /**
     * Adds a new execution to the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionName The string execution name to set.
     * @return The new execution that was added.
     * @throws WebAppException If a new execution cannot be added.
     */
    public Execution addExecution(Long userId, Long compositeId, String executionName) throws WebAppException {

        Composite composite;
        Execution execution;
        ExecutionRunner executionRunner;
        String exceptionTitle = "Add Execution Failure";

        try {

            composite = compositeManager.getComposite(userId, compositeId);
            validateExecutableState(userId, composite);
            validateExecutionName(userId, compositeId, executionName);
            executionRunner = ExecutionRunner.createInstance();
            execution = executionRunner.start(userId, composite, executionName);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        executionMap.put(execution.getId(), executionRunner);

        return execution;
    }

    /**
     * Validates the executable state of the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param composite The composite to validate.
     * @throws WebAppException If the composite is not executable.
     */
    public void validateExecutableState(Long userId, Composite composite) throws WebAppException {

        String exceptionTitle = "Validate Executable State Failure";

        if (composite.getSimulations().isEmpty()) {

            throw new WebAppException(exceptionTitle, "Composites must have at least one simulation to be executed.");
        }

        if (!composite.isExecutable()) {

            throw new WebAppException(exceptionTitle, String.format("The composite with ID \"%d\" cannot be executed. The most likely reason is that the composite or one of its simulations is " +
                    "configured to use an application that has been removed.", composite.getId()));
        }

        List<Execution> executions = entityManager.createNamedQuery(Query.EXECUTIONS_FROM_USER_ID_EXECUTION_STATUS, Execution.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.EXECUTION_STATUS, ExecutionStatus.IN_PROGRESS)
                .getResultList();

        if (!executions.isEmpty()) {

            throw new WebAppException(exceptionTitle, "New executions cannot be started while unfinished executions are present.");
        }
    }

    /**
     * Removes the specified execution from the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @throws WebAppException If the execution cannot be removed.
     */
    public void removeExecution(Long userId, Long compositeId, Long executionId) throws WebAppException {

        try {

            removeExecutions(userId, compositeId, Arrays.asList(executionId));
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove Execution Failure", exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Removes the specified executions from the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionIds The list of IDs of the executions.
     * @throws WebAppException If the executions cannot be removed.
     */
    public void removeExecutions(Long userId, Long compositeId, List<Long> executionIds) throws WebAppException {

        List<Execution> executions;

        try {

            executions = buildExecutionList(getExecutions(userId, compositeId), executionIds);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove Executions Failure", exception.getMessage(), exception.getStatus());
        }

        for (Execution execution : executions) {

            if (executionMap.get(execution.getId()) != null) {

                executionMap.get(execution.getId()).deleteExecution();
            }
            else {

                entityManager.remove(execution);
            }
        }

        entityManager.createNamedQuery(Query.DELETE_COMMANDS_FROM_EXECUTION_LIST)
                .setParameter(QueryParameter.EXECUTION_LIST, executionIds)
                .executeUpdate();

        entityManager.createNamedQuery(Query.DELETE_APPLICATION_LOGS_FROM_EXECUTION_LIST)
                .setParameter(QueryParameter.EXECUTION_LIST, executionIds)
                .executeUpdate();

        entityManager.flush();
        entityManager.clear();
    }

    /**
     * Returns a list of executions from the source list with the specified IDs.
     * 
     * @param executionList The list of source executions.
     * @param executionIds The list of execution IDs to include.
     * @return A list of executions from the source list with the specified IDs.
     * @throws WebAppException If nonexistent or duplicate IDs are provided.
     */
    private List<Execution> buildExecutionList(List<Execution> executionList, List<Long> executionIds) throws WebAppException {

        HashMap<Long, Execution> executionMapping = new HashMap<>();

        for (Execution execution : executionList) {

            if (executionIds.contains(execution.getId())) {

                executionMapping.put(execution.getId(), execution);
            }
        }

        ArrayList<Execution> executions = new ArrayList<>(executionMapping.values());

        for (Long id : executionIds) {

            Execution execution = executionMapping.get(id);

            if (execution == null) {

                String message = String.format("No execution with ID \"%d\" could be found in the composite.", id);
                Response.Status status = Response.Status.NOT_FOUND;

                if (executionMapping.containsKey(id)) {

                    message = "The same execution ID may not be listed twice.";
                    status = Response.Status.CONFLICT;
                }

                throw new WebAppException("Build Execution List Failure", message, status);
            }

            executionMapping.put(id, null);
        }

        return executions;
    }

    /**
     * Returns the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return The execution with the specified attributes.
     * @throws WebAppException If no such execution exists.
     */
    public Execution getExecution(Long userId, Long compositeId, Long executionId) throws WebAppException {

        String exceptionTitle = "Get Execution Failure";

        try {

            compositeManager.getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<Execution> executions = entityManager.createNamedQuery(Query.EXECUTIONS_FROM_USER_ID_COMPOSITE_ID_EXECUTION_ID, Execution.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.EXECUTION_ID, executionId)
                .getResultList();

        if (executions.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No execution with ID \"%d\" could be found in the composite.", executionId), Response.Status.NOT_FOUND);
        }

        return executions.get(0);
    }

    /**
     * Updates the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param executionName The string execution name to set.
     * @param steps The long number of steps to advance the execution.
     * @throws WebAppException If the execution cannot be updated.
     */
    public void updateExecution(Long userId, Long compositeId, Long executionId, String executionName, Long steps) throws WebAppException {

        Execution execution;

        try {

            execution = getExecution(userId, compositeId, executionId);
            executionName = (executionName != null) ? executionName : execution.getName();
            steps = (steps != null) ? steps : 0L;

            if (!execution.getName().equalsIgnoreCase(executionName)) {

                validateExecutionName(userId, compositeId, executionName);
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException("Update Execution Failure", exception.getMessage(), exception.getStatus());
        }

        if (steps > 0) {

            advanceExecution(execution, steps);
        }

        execution.setName(executionName);
    }

    /**
     * Advances the specified execution by the specified number of steps.
     * 
     * @param execution The execution to advance.
     * @param steps The long number of steps to advance.
     * @throws WebAppException If the execution cannot be advanced.
     */
    private void advanceExecution(Execution execution, Long steps) throws WebAppException {

        long stepsRemaining = execution.getMaximumSteps() - execution.getStepNumber();
        steps = Math.min(steps, stepsRemaining);

        ExecutionRunner executionRunner = executionMap.get(execution.getId());

        if (executionRunner == null) {

            throw new WebAppException("Advance Execution Failure", String.format("The execution with ID \"%d\" is not running and may not be advanced.", execution.getId()));
        }

        executionRunner.stepExecution(steps.intValue());

        if (execution.getStatus() != ExecutionStatus.IN_PROGRESS) {

            executionMap.remove(execution.getId());
        }
    }

    /**
     * Validates the specified execution name.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionName The string execution name to validate.
     * @throws WebAppException If the execution name is not unique.
     */
    public void validateExecutionName(Long userId, Long compositeId, String executionName) throws WebAppException {

        List<Execution> executions = entityManager.createNamedQuery(Query.EXECUTIONS_FROM_USER_ID_COMPOSITE_ID_EXECUTION_NAME, Execution.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.EXECUTION_NAME, executionName)
                .getResultList();

        if (!executions.isEmpty()) {

            throw new WebAppException("Validate Execution Name Failure", String.format("An execution with the name \"%s\" already exists in the composite.", executionName));
        }
    }

    /**
     * Returns the unique application names from application logs for the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param deviceList The list of host devices (MAC addresses) to consider in the search.
     *        Providing a <code>null</code> or empty list will result in the inclusion of all valid
     *        host devices.
     * @return A list of unique application names from application logs for the specified execution.
     * @throws WebAppException If the unique application names cannot be retrieved.
     */
    public List<String> getApplicationLogApplications(Long userId, Long compositeId, Long executionId, List<Long> deviceList) throws WebAppException {

        try {

            getExecution(userId, compositeId, executionId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Application Log Applications Failure", exception.getMessage(), exception.getStatus());
        }

        if (deviceList == null || deviceList.isEmpty()) {

            return entityManager.createNamedQuery(Query.APPLICATION_LOG_APPLICATIONS_FROM_EXECUTION_ID, String.class)
                    .setParameter(QueryParameter.EXECUTION_ID, executionId)
                    .getResultList();
        }

        return entityManager.createNamedQuery(Query.APPLICATION_LOG_APPLICATIONS_FROM_EXECUTION_ID_DEVICE_LIST, String.class)
                .setParameter(QueryParameter.EXECUTION_ID, executionId)
                .setParameter(QueryParameter.DEVICE_LIST, deviceList)
                .getResultList();
    }

    /**
     * Returns the number of application logs for the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param deviceList The list of host devices (MAC addresses) to consider in the search.
     *        Providing a <code>null</code> or empty list will result in the inclusion of all valid
     *        host devices.
     * @param applicationList The list of applications to consider in the search. Providing a
     *        <code>null</code> or empty list will result in the inclusion of all valid
     *        applications.
     * @param keyList The list of application keys to consider in the search. Providing a
     *        <code>null</code> or empty list will result in the inclusion of all valid application
     *        keys.
     * @param minTime The double minimum simulation time to consider in the search. Providing a
     *        <code>null</code> value will result in the use of the absolute minimum simulation
     *        time.
     * @param maxTime The double maximum simulation time to consider in the search. Providing a
     *        <code>null</code> value will result in the use of the absolute maximum simulation
     *        time.
     * @return The long number of application logs for the specified execution.
     * @throws WebAppException If the number of application logs cannot be retrieved.
     */
    public long getApplicationLogCount(Long userId, Long compositeId, Long executionId, List<Long> deviceList, List<String> applicationList, List<String> keyList, Double minTime, Double maxTime)
            throws WebAppException {

        javax.persistence.Query query;

        try {

            query = buildLogsQuery(userId, compositeId, executionId, deviceList, applicationList, keyList, minTime, maxTime, true);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Application Log Count Failure", exception.getMessage(), exception.getStatus());
        }

        return (long)query.getSingleResult();
    }

    /**
     * Returns the unique device IDs from application logs for the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A list of unique device IDs from application logs for the specified execution.
     * @throws WebAppException If the unique device IDs cannot be retrieved.
     */
    public List<Long> getApplicationLogDevices(Long userId, Long compositeId, Long executionId) throws WebAppException {

        try {

            getExecution(userId, compositeId, executionId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Application Log Devices Failure", exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.APPLICATION_LOG_DEVICES_FROM_EXECUTION_ID, Long.class)
                .setParameter(QueryParameter.EXECUTION_ID, executionId)
                .getResultList();
    }

    /**
     * Returns the unique keys from application logs for the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param deviceList The list of host devices (MAC addresses) to consider in the search.
     *        Providing a <code>null</code> or empty list will result in the inclusion of all valid
     *        host devices.
     * @param applicationList The list of applications to consider in the search. Providing a
     *        <code>null</code> or empty list will result in the inclusion of all valid
     *        applications.
     * @return A list of unique keys from application logs for the specified execution.
     * @throws WebAppException If the unique keys cannot be retrieved.
     */
    public List<String> getApplicationLogKeys(Long userId, Long compositeId, Long executionId, List<Long> deviceList, List<String> applicationList) throws WebAppException {

        try {

            getExecution(userId, compositeId, executionId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Application Log Keys Failure", exception.getMessage(), exception.getStatus());
        }

        if (deviceList == null || deviceList.isEmpty()) {

            if (applicationList == null || applicationList.isEmpty()) {

                return entityManager.createNamedQuery(Query.APPLICATION_LOG_KEYS_FROM_EXECUTION_ID, String.class)
                        .setParameter(QueryParameter.EXECUTION_ID, executionId)
                        .getResultList();
            }

            return entityManager.createNamedQuery(Query.APPLICATION_LOG_KEYS_FROM_EXECUTION_ID_APPLICATION_LIST, String.class)
                    .setParameter(QueryParameter.EXECUTION_ID, executionId)
                    .setParameter(QueryParameter.APPLICATION_LIST, applicationList)
                    .getResultList();
        }
        else if (applicationList == null || applicationList.isEmpty()) {

            return entityManager.createNamedQuery(Query.APPLICATION_LOG_KEYS_FROM_EXECUTION_ID_DEVICE_LIST, String.class)
                    .setParameter(QueryParameter.EXECUTION_ID, executionId)
                    .setParameter(QueryParameter.DEVICE_LIST, deviceList)
                    .getResultList();
        }

        return entityManager.createNamedQuery(Query.APPLICATION_LOG_KEYS_FROM_EXECUTION_ID_DEVICE_LIST_APPLICATION_LIST, String.class)
                .setParameter(QueryParameter.EXECUTION_ID, executionId)
                .setParameter(QueryParameter.DEVICE_LIST, deviceList)
                .setParameter(QueryParameter.APPLICATION_LIST, applicationList)
                .getResultList();
    }

    /**
     * Returns the application logs for the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param deviceList The list of host devices (MAC addresses) to consider in the search.
     *        Providing a <code>null</code> or empty list will result in the inclusion of all valid
     *        host devices.
     * @param applicationList The list of applications to consider in the search. Providing a
     *        <code>null</code> or empty list will result in the inclusion of all valid
     *        applications.
     * @param keyList The list of application keys to consider in the search. Providing a
     *        <code>null</code> or empty list will result in the inclusion of all valid application
     *        keys.
     * @param minTime The double minimum simulation time to consider in the search. Providing a
     *        <code>null</code> value will result in the use of the absolute minimum simulation
     *        time.
     * @param maxTime The double maximum simulation time to consider in the search. Providing a
     *        <code>null</code> value will result in the use of the absolute maximum simulation
     *        time.
     * @param start The index to start retrieving application logs.
     * @param limit The maximum number of application logs to retrieve.
     * @return A list of application logs for the specified execution.
     * @throws WebAppException If the application logs cannot be retrieved.
     */
    @SuppressWarnings("unchecked")
    public List<ApplicationLog> getApplicationLogs(Long userId, Long compositeId, Long executionId, List<Long> deviceList, List<String> applicationList, List<String> keyList, Double minTime,
            Double maxTime, int start, int limit) throws WebAppException {

        javax.persistence.Query query;

        try {

            query = buildLogsQuery(userId, compositeId, executionId, deviceList, applicationList, keyList, minTime, maxTime, false);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Application Logs Failure", exception.getMessage(), exception.getStatus());
        }

        return (List<ApplicationLog>)query.setFirstResult(start).setMaxResults(limit).getResultList();
    }

    /**
     * Returns an application logs query constructed with the specified search filters.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param deviceList The list of host devices (MAC addresses) to consider in the search.
     *        Providing a <code>null</code> or empty list will result in the inclusion of all valid
     *        host devices.
     * @param applicationList The list of applications to consider in the search. Providing a
     *        <code>null</code> or empty list will result in the inclusion of all valid
     *        applications.
     * @param keyList The list of application keys to consider in the search. Providing a
     *        <code>null</code> or empty list will result in the inclusion of all valid application
     *        keys.
     * @param minTime The double minimum simulation time to consider in the search. Providing a
     *        <code>null</code> value will result in the use of the absolute minimum simulation
     *        time.
     * @param maxTime The double maximum simulation time to consider in the search. Providing a
     *        <code>null</code> value will result in the use of the absolute maximum simulation
     *        time.
     * @param queryName The name of the query to construct.
     * @return An application logs query with the specified search filters.
     * @throws WebAppException If the application logs query cannot be constructed.
     */
    private javax.persistence.Query buildLogsQuery(Long userId, Long compositeId, Long executionId, List<Long> deviceList, List<String> applicationList, List<String> keyList, Double minTime,
            Double maxTime, boolean isCountQuery) throws WebAppException {

        try {

            Execution execution = getExecution(userId, compositeId, executionId);
            minTime = (minTime != null) ? minTime : 0.0;
            maxTime = (maxTime != null) ? maxTime : execution.getStepSize() * execution.getMaximumSteps();
            validateTimeRange(execution, minTime, maxTime);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Build Logs Query Failure", exception.getMessage(), exception.getStatus());
        }

        StringBuilder typeStringBuilder = new StringBuilder();
        typeStringBuilder.append((deviceList == null || deviceList.isEmpty()) ? '0' : '1');
        typeStringBuilder.append((applicationList == null || applicationList.isEmpty()) ? '0' : '1');
        typeStringBuilder.append((keyList == null || keyList.isEmpty()) ? '0' : '1');
        int queryType = Integer.parseInt(typeStringBuilder.toString(), 2);

        javax.persistence.Query query;

        switch (queryType) {

            case 0:
                query = isCountQuery ? entityManager.createNamedQuery(Query.APPLICATION_LOG_COUNT_FROM_EXECUTION_ID_MIN_TIME_MAX_TIME)
                        : entityManager.createNamedQuery(Query.APPLICATION_LOGS_FROM_EXECUTION_ID_MIN_TIME_MAX_TIME, ApplicationLog.class);
                break;

            case 1:
                query = isCountQuery ? entityManager.createNamedQuery(Query.APPLICATION_LOG_COUNT_FROM_EXECUTION_ID_KEY_LIST_MIN_TIME_MAX_TIME)
                        : entityManager.createNamedQuery(Query.APPLICATION_LOGS_FROM_EXECUTION_ID_KEY_LIST_MIN_TIME_MAX_TIME, ApplicationLog.class);
                query = query.setParameter(QueryParameter.KEY_LIST, keyList);
                break;

            case 2:
                query = isCountQuery ? entityManager.createNamedQuery(Query.APPLICATION_LOG_COUNT_FROM_EXECUTION_ID_APPLICATION_LIST_MIN_TIME_MAX_TIME)
                        : entityManager.createNamedQuery(Query.APPLICATION_LOGS_FROM_EXECUTION_ID_APPLICATION_LIST_MIN_TIME_MAX_TIME, ApplicationLog.class);
                query = query.setParameter(QueryParameter.APPLICATION_LIST, applicationList);
                break;

            case 3:
                query = isCountQuery ? entityManager.createNamedQuery(Query.APPLICATION_LOG_COUNT_FROM_EXECUTION_ID_APPLICATION_LIST_KEY_LIST_MIN_TIME_MAX_TIME)
                        : entityManager.createNamedQuery(Query.APPLICATION_LOGS_FROM_EXECUTION_ID_APPLICATION_LIST_KEY_LIST_MIN_TIME_MAX_TIME, ApplicationLog.class);
                query = query.setParameter(QueryParameter.APPLICATION_LIST, applicationList)
                        .setParameter(QueryParameter.KEY_LIST, keyList);
                break;

            case 4:
                query = isCountQuery ? entityManager.createNamedQuery(Query.APPLICATION_LOG_COUNT_FROM_EXECUTION_ID_DEVICE_LIST_MIN_TIME_MAX_TIME)
                        : entityManager.createNamedQuery(Query.APPLICATION_LOGS_FROM_EXECUTION_ID_DEVICE_LIST_MIN_TIME_MAX_TIME, ApplicationLog.class);
                query = query.setParameter(QueryParameter.DEVICE_LIST, deviceList);
                break;

            case 5:
                query = isCountQuery ? entityManager.createNamedQuery(Query.APPLICATION_LOG_COUNT_FROM_EXECUTION_ID_DEVICE_LIST_KEY_LIST_MIN_TIME_MAX_TIME)
                        : entityManager.createNamedQuery(Query.APPLICATION_LOGS_FROM_EXECUTION_ID_DEVICE_LIST_KEY_LIST_MIN_TIME_MAX_TIME, ApplicationLog.class);
                query = query.setParameter(QueryParameter.DEVICE_LIST, deviceList)
                        .setParameter(QueryParameter.KEY_LIST, keyList);
                break;

            case 6:
                query = isCountQuery ? entityManager.createNamedQuery(Query.APPLICATION_LOG_COUNT_FROM_EXECUTION_ID_DEVICE_LIST_APPLICATION_LIST_MIN_TIME_MAX_TIME)
                        : entityManager.createNamedQuery(Query.APPLICATION_LOGS_FROM_EXECUTION_ID_DEVICE_LIST_APPLICATION_LIST_MIN_TIME_MAX_TIME, ApplicationLog.class);
                query = query.setParameter(QueryParameter.DEVICE_LIST, deviceList)
                        .setParameter(QueryParameter.APPLICATION_LIST, applicationList);
                break;

            case 7:
                query = isCountQuery ? entityManager.createNamedQuery(Query.APPLICATION_LOG_COUNT_FROM_EXECUTION_ID_DEVICE_LIST_APPLICATION_LIST_KEY_LIST_MIN_TIME_MAX_TIME)
                        : entityManager.createNamedQuery(Query.APPLICATION_LOGS_FROM_EXECUTION_ID_DEVICE_LIST_APPLICATION_LIST_KEY_LIST_MIN_TIME_MAX_TIME, ApplicationLog.class);
                query = query.setParameter(QueryParameter.DEVICE_LIST, deviceList)
                        .setParameter(QueryParameter.APPLICATION_LIST, applicationList)
                        .setParameter(QueryParameter.KEY_LIST, keyList);
                break;

            default:
                throw new IllegalStateException(String.format("The value \"%d\" is not a valid query type.", queryType));
        }

        return query.setParameter(QueryParameter.EXECUTION_ID, executionId)
                .setParameter(QueryParameter.MIN_TIME, minTime)
                .setParameter(QueryParameter.MAX_TIME, maxTime);
    }

    /**
     * Validates the specified time range.
     * 
     * @param execution The target execution.
     * @param minTime The double minimum simulation time to validate.
     * @param maxTime The double maximum simulation time to validate.
     * @throws WebAppException If the time range is not valid.
     */
    private void validateTimeRange(Execution execution, Double minTime, Double maxTime) throws WebAppException {

        String exceptionTitle = "Validate Time Range Failure";

        if (minTime < 0.0) {

            throw new WebAppException(exceptionTitle, "The minimum simulation time may not be less than 0 seconds.");
        }

        double totalTime = execution.getMaximumSteps() * execution.getStepSize();
        if (maxTime > totalTime) {

            throw new WebAppException(exceptionTitle, String.format("The maximum simulation time may not be more than %f seconds.", totalTime));
        }

        if (minTime > maxTime) {

            throw new WebAppException(exceptionTitle, "The minimum simulation time may not exceed the maximum simulation time.");
        }
    }

    /**
     * Persists a batch of database entities. Persisted entities must be independent (i.e. not
     * children) of other database entities.
     * 
     * @param entities The list of entities to persist.
     */
    public void batch(List<? extends AbstractEntity> entities) {

        if (entities == null || entities.isEmpty()) {

            return;
        }

        if (batchSize == 0) {

            batchSize = Integer.parseInt((String)entityManager.getEntityManagerFactory().getProperties().get("hibernate.jdbc.batch_size"));
        }

        for (int i = 0; i < entities.size(); i++) {

            entityManager.persist(entities.get(i));

            if ((i + 1) % batchSize == 0) {

                entityManager.flush();
                entityManager.clear();
            }
        }

        entityManager.flush();
        entityManager.clear();
    }

    /**
     * Returns the commands for the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A list of commands for the specified execution.
     * @throws WebAppException If the commands cannot be retrieved.
     */
    public List<Command> getCommands(Long userId, Long compositeId, Long executionId) throws WebAppException {

        try {

            getExecution(userId, compositeId, executionId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Commands Failure", exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.COMMANDS_FROM_EXECUTION_ID, Command.class)
                .setParameter(QueryParameter.EXECUTION_ID, executionId)
                .getResultList();
    }

    /**
     * Adds a new injection command to the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param simulationId The long ID of the simulation.
     * @param laneId The integer ID of the lane.
     * @param speed The double speed (m/s).
     * @throws WebAppException If a new injection command cannot be added.
     */
    public void addInjectionCommand(Long userId, Long compositeId, Long executionId, Long simulationId, Integer laneId, Double speed) throws WebAppException {

        Composite composite;
        String exceptionTitle = "Add Injection Command Failure";

        try {

            composite = compositeManager.getComposite(userId, compositeId);
            getExecution(userId, compositeId, executionId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        ExecutionRunner executionRunner = executionMap.get(executionId);

        if (executionRunner == null) {

            throw new WebAppException(exceptionTitle, String.format("The execution with ID \"%d\" is not running and new injection commands cannot be added.", executionId));
        }

        try {

            validateSimulation(composite, simulationId);
            validateLane(userId, compositeId, simulationId, laneId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        executionRunner.addInjectionCommand(simulationId, laneId, speed);
    }

    /**
     * Adds a new lane command to the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param simulationId The long ID of the simulation.
     * @param vehicleId The integer ID of the vehicle.
     * @param command The integer command type.
     * @throws WebAppException If a new lane command cannot be added.
     */
    public void addLaneCommand(Long userId, Long compositeId, Long executionId, Long simulationId, Integer vehicleId, Integer command) throws WebAppException {

        Composite composite;
        String exceptionTitle = "Add Lane Command Failure";

        try {

            composite = compositeManager.getComposite(userId, compositeId);
            getExecution(userId, compositeId, executionId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        ExecutionRunner executionRunner = executionMap.get(executionId);

        if (executionRunner == null) {

            throw new WebAppException(exceptionTitle, String.format("The execution with ID \"%d\" is not running and new lane commands cannot be added.", executionId));
        }

        try {

            validateSimulation(composite, simulationId);
            validateVehicle(executionRunner, userId, compositeId, simulationId, vehicleId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        executionRunner.addLaneCommand(simulationId, vehicleId, command);
    }

    /**
     * Adds a new signal command to the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param simulationId The long ID of the simulation.
     * @param command The integer command type.
     * @param time The double signal time (s).
     * @throws WebAppException If a new signal command cannot be added.
     */
    public void addSignalCommand(Long userId, Long compositeId, Long executionId, Long simulationId, Integer command, Double time) throws WebAppException {

        Composite composite;
        String exceptionTitle = "Add Signal Command Failure";

        try {

            composite = compositeManager.getComposite(userId, compositeId);
            getExecution(userId, compositeId, executionId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        ExecutionRunner executionRunner = executionMap.get(executionId);

        if (executionRunner == null) {

            throw new WebAppException(exceptionTitle, String.format("The execution with ID \"%d\" is not running and new signal commands cannot be added.", executionId));
        }

        try {

            validateSimulation(composite, simulationId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        executionRunner.addSignalCommand(simulationId, command, time);
    }

    /**
     * Adds a new speed command to the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param simulationId The long ID of the simulation.
     * @param vehicleId The integer ID of the vehicle.
     * @param command The integer command type.
     * @param speed The double speed (m/s).
     * @throws WebAppException If a new speed command cannot be added.
     */
    public void addSpeedCommand(Long userId, Long compositeId, Long executionId, Long simulationId, Integer vehicleId, Integer command, Double speed) throws WebAppException {

        Composite composite;
        String exceptionTitle = "Add Speed Command Failure";

        try {

            composite = compositeManager.getComposite(userId, compositeId);
            getExecution(userId, compositeId, executionId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        ExecutionRunner executionRunner = executionMap.get(executionId);

        if (executionRunner == null) {

            throw new WebAppException(exceptionTitle, String.format("The execution with ID \"%d\" is not running and new speed commands cannot be added.", executionId));
        }

        try {

            validateSimulation(composite, simulationId);
            validateVehicle(executionRunner, userId, compositeId, simulationId, vehicleId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        executionRunner.addSpeedCommand(simulationId, vehicleId, command, speed);
    }

    /**
     * Validates the specified lane.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param laneId The integer ID of the lane.
     * @throws WebAppException If the lane is not valid.
     */
    private void validateLane(Long userId, Long compositeId, Long simulationId, Integer laneId) throws WebAppException {

        ILane lane = null;
        String exceptionTitle = "Validate Lane Failure";

        for (ILane simulationLane : compositeManager.getLanes(userId, compositeId).get(simulationId)) {

            if (simulationLane.getLaneId() == laneId) {

                lane = simulationLane;
                break;
            }
        }

        if (lane == null) {

            throw new WebAppException(exceptionTitle, String.format("No lane with ID \"%d\" could be found in the simulation.", laneId), Response.Status.NOT_FOUND);
        }

        if (!lane.getType().equals(Lane.INBOUND)) {

            throw new WebAppException(exceptionTitle, String.format("Lane \"%d\" is not an inbound lane.", laneId));
        }
    }

    /**
     * Validates the specified simulation.
     * 
     * @param composite The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @throws WebAppException If the simulation is not in the specified composite.
     */
    private void validateSimulation(Composite composite, Long simulationId) throws WebAppException {

        for (Simulation simulation : composite.getSimulations()) {

            if (simulation.getId().equals(simulationId)) {

                return;
            }
        }

        throw new WebAppException("Validate Simulation Failure", String.format("No simulation with ID \"%d\" could be found in the composite.", simulationId), Response.Status.NOT_FOUND);
    }

    /**
     * Validates the specified vehicle.
     * 
     * @param executionRunner The execution runner to check.
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param vehicleId The long ID of the vehicle to validate.
     * @throws WebAppException If the vehicle does not exist or is in an outbound lane.
     */
    private void validateVehicle(ExecutionRunner executionRunner, Long userId, Long compositeId, Long simulationId, Integer vehicleId) throws WebAppException {

        Vehicle vehicle = null;
        String exceptionTitle = "Validate Vehicle Failure";

        List<Vehicle> vehicles = executionRunner.getVehicles().get(simulationId);

        for (Vehicle simulationVehicle : vehicles) {

            if (simulationVehicle.getVehicleID() == vehicleId) {

                vehicle = simulationVehicle;
                break;
            }
        }

        if (vehicle == null) {

            throw new WebAppException(exceptionTitle, String.format("No vehicle with ID \"%d\" could be found in the simulation.", vehicleId), Response.Status.NOT_FOUND);
        }

        try {

            validateLane(userId, compositeId, simulationId, vehicle.getLaneID());
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Returns the detectors for the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A map of detectors for the specified execution.
     * @throws WebAppException If the detectors cannot be retrieved.
     */
    public Map<Long, List<IDetector>> getDetectors(Long userId, Long compositeId, Long executionId) throws WebAppException {

        String exceptionTitle = "Get Detectors Failure";

        try {

            getExecution(userId, compositeId, executionId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        ExecutionRunner executionRunner = executionMap.get(executionId);

        if (executionRunner == null) {

            throw new WebAppException(exceptionTitle, String.format("The execution with ID \"%d\" is not running and detector information is not available.", executionId));
        }

        return executionRunner.getDetectors();
    }

    /**
     * Returns the messages for the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A list of messages for the specified execution.
     * @throws WebAppException If the messages cannot be retrieved.
     */
    public List<MessageData> getMessages(Long userId, Long compositeId, Long executionId) throws WebAppException {

        String exceptionTitle = "Get Messages Failure";

        try {

            getExecution(userId, compositeId, executionId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        ExecutionRunner executionRunner = executionMap.get(executionId);

        if (executionRunner == null) {

            throw new WebAppException(exceptionTitle, String.format("The execution with ID \"%d\" is not running and message information is not available.", executionId));
        }

        return executionRunner.getMessages();
    }

    /**
     * Returns the messages for the specified device.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param deviceMac The MAC address of the device.
     * @return A list of messages for the specified device.
     * @throws WebAppException If the messages cannot be retrieved.
     */
    public List<MessageData> getMessagesByDevice(Long userId, Long compositeId, Long executionId, Long deviceMac) throws WebAppException {

        String exceptionTitle = "Get Messages By Device Failure";

        try {

            getExecution(userId, compositeId, executionId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        ExecutionRunner executionRunner = executionMap.get(executionId);

        if (executionRunner == null) {

            throw new WebAppException(exceptionTitle, String.format("The execution with ID \"%d\" is not running and message information is not available.", executionId));
        }

        return executionRunner.getMessagesByDevice(deviceMac);
    }

    /**
     * Returns the signal indications for the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A map of signal indications for the specified execution.
     * @throws WebAppException If the signal indications cannot be retrieved.
     */
    public Map<Long, List<ISignalIndication>> getSignalIndications(Long userId, Long compositeId, Long executionId) throws WebAppException {

        String exceptionTitle = "Get Signal Indications Failure";

        try {

            getExecution(userId, compositeId, executionId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        ExecutionRunner executionRunner = executionMap.get(executionId);

        if (executionRunner == null) {

            throw new WebAppException(exceptionTitle, String.format("The execution with ID \"%d\" is not running and signal information is not available.", executionId));
        }

        return executionRunner.getSignalIndications();
    }

    /**
     * Returns the on board devices for the specified vehicle.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param vehicleId The long ID of the vehicle.
     * @return A list of on board devices for the specified vehicle.
     * @throws WebAppException If the on board devices cannot be retrieved.
     */
    public List<OnBoardDevice> getOnBoardDevices(Long userId, Long compositeId, Long executionId, Long vehicleId) throws WebAppException {

        String exceptionTitle = "Get On Board Devices Failure";

        try {

            getExecution(userId, compositeId, executionId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        ExecutionRunner executionRunner = executionMap.get(executionId);

        if (executionRunner == null) {

            throw new WebAppException(exceptionTitle, String.format("The execution with ID \"%d\" is not running and on board device information is not available.", executionId));
        }

        return executionRunner.getOnBoardDevices(vehicleId);
    }

    /**
     * Returns the standalone devices for the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A list of standalone devices for the specified execution.
     * @throws WebAppException If the standalone devices cannot be retrieved.
     */
    public List<StandaloneDevice> getStandaloneDevices(Long userId, Long compositeId, Long executionId) throws WebAppException {

        String exceptionTitle = "Get Standalone Devices Failure";

        try {

            getExecution(userId, compositeId, executionId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        ExecutionRunner executionRunner = executionMap.get(executionId);

        if (executionRunner == null) {

            throw new WebAppException(exceptionTitle, String.format("The execution with ID \"%d\" is not running and standalone device information is not available.", executionId));
        }

        return executionRunner.getStandaloneDevices();
    }

    /**
     * Returns the vehicles for the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A map of vehicles for the specified execution.
     * @throws WebAppException If the vehicles cannot be retrieved.
     */
    public Map<Long, List<Vehicle>> getVehicles(Long userId, Long compositeId, Long executionId) throws WebAppException {

        String exceptionTitle = "Get Vehicles Failure";

        try {

            getExecution(userId, compositeId, executionId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        ExecutionRunner executionRunner = executionMap.get(executionId);

        if (executionRunner == null) {

            throw new WebAppException(exceptionTitle, String.format("The execution with ID \"%d\" is not running and vehicle information is not available.", executionId));
        }

        return executionRunner.getVehicles();
    }

    /**
     * Returns the execution messages for the specified execution.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A list of execution messages for the specified execution.
     * @throws WebAppException If the execution messages cannot be retrieved.
     */
    public List<ExecutionMessage> getExecutionMessages(Long userId, Long compositeId, Long executionId) throws WebAppException {

        String exceptionTitle = "Get Execution Messages Failure";

        try {

            getExecution(userId, compositeId, executionId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.EXECUTION_MESSAGES_FROM_USER_ID_COMPOSITE_ID_EXECUTION_ID, ExecutionMessage.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.EXECUTION_ID, executionId)
                .getResultList();
    }

    /** Updates the status of executions that were in progress prior to a server restart/failure. */
    public void updateStaleExecutions() {

        entityManager.createNamedQuery(Query.UPDATE_STALE_EXECUTIONS).executeUpdate();
    }
}
