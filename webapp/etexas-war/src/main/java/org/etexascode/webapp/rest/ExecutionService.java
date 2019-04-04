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
package org.etexascode.webapp.rest;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.interceptor.Interceptors;
import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;

import org.apache.commons.io.FileUtils;
import org.etexascode.webapp.cdi.CurrentUser;
import org.etexascode.webapp.datamodel.ApplicationLog;
import org.etexascode.webapp.datamodel.Execution;
import org.etexascode.webapp.ejb.ExecutionManager;
import org.etexascode.webapp.exception.WebAppException;
import org.etexascode.webapp.rest.validation.ExecutionIdListValidator;
import org.etexascode.webapp.rest.validation.ExecutionNameValidator;
import org.etexascode.webapp.rest.validation.ExecutionStepsValidator;
import org.etexascode.webapp.rest.validation.LaneCommandValidator;
import org.etexascode.webapp.rest.validation.LaneIdValidator;
import org.etexascode.webapp.rest.validation.RestValidator;
import org.etexascode.webapp.rest.validation.SignalCommandValidator;
import org.etexascode.webapp.rest.validation.SimulationIdValidator;
import org.etexascode.webapp.rest.validation.SpeedCommandValidator;
import org.etexascode.webapp.rest.validation.SpeedValidator;
import org.etexascode.webapp.rest.validation.TimeValidator;
import org.etexascode.webapp.rest.validation.VehicleIdValidator;

/**
 * The REST service for execution operation requests.
 * 
 * @author bbadillo
 * @author jrutherford
 * @author keagan
 * @author emyers
 * @author ttevendale
 */
@RequestScoped
@Path("/api/composites/{compositeId}/executions")
@Interceptors({ StringTrimmer.class })
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public class ExecutionService {

    /** The current user. */
    @Inject
    private CurrentUser user;

    /** The execution transaction manager. */
    @Inject
    private ExecutionManager executionManager;

    /**
     * Returns the executions for the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @return A response (200) with the executions for the specified composite.
     * @throws WebAppException If the executions cannot be retrieved.
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public Response getExecutions(@PathParam("compositeId") final Long compositeId) throws WebAppException {

        return Response.ok(executionManager.getExecutions(user.getId(), compositeId)).build();
    }

    /**
     * Removes the specified executions from the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionIds The list of IDs of the executions.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the executions cannot be removed.
     */
    @DELETE
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeExecutions(@PathParam("compositeId") final Long compositeId, @QueryParam("executionIds") final List<Long> executionIds) throws WebAppException {

        RestValidator.validate(new ExecutionIdListValidator(executionIds));
        executionManager.removeExecutions(user.getId(), compositeId, executionIds);
        return Response.noContent().build();
    }

    /**
     * Adds a new execution to the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionName The string execution name to set.
     * @return A response (201) with the new execution that was added.
     * @throws URISyntaxException If a new execution cannot be added.
     * @throws WebAppException If a new execution cannot be added.
     */
    @POST
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addExecution(@PathParam("compositeId") final Long compositeId, @FormParam("executionName") final String executionName) throws URISyntaxException, WebAppException {

        RestValidator.validate(new ExecutionNameValidator(executionName));
        Execution execution = executionManager.addExecution(user.getId(), compositeId, executionName);
        URI uri = new URI(String.format("/api/composites/%d/executions/%d", compositeId, execution.getId()));
        return Response.created(uri).entity(execution).build();
    }

    /**
     * Returns the specified execution.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A response (200) with the specified execution.
     * @throws WebAppException If the execution cannot be retrieved.
     */
    @GET
    @Path("/{executionId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getExecution(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId) throws WebAppException {

        return Response.ok(executionManager.getExecution(user.getId(), compositeId, executionId)).build();
    }

    /**
     * Removes the specified execution from the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the execution cannot be removed.
     */
    @DELETE
    @Path("/{executionId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeExecution(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId) throws WebAppException {

        executionManager.removeExecution(user.getId(), compositeId, executionId);
        return Response.noContent().build();
    }

    /**
     * Updates the specified execution.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param executionName The string execution name to set.
     * @param steps The long number of steps to advance the execution.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the execution cannot be updated.
     */
    @PUT
    @Path("/{executionId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateExecution(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId, @FormParam("executionName") final String executionName,
            @FormParam("steps") final Long steps) throws WebAppException {

        RestValidator.validateOptional(
                new ExecutionNameValidator(executionName),
                new ExecutionStepsValidator(steps));

        executionManager.updateExecution(user.getId(), compositeId, executionId, executionName, steps);
        return Response.noContent().build();
    }

    /**
     * Returns the application logs for the specified execution.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param deviceList the device IDs of interest
     * @param applicationList the application names of interest
     * @param keyList the message keys of interest
     * @param minTime the minimum simulation time
     * @param maxTime the maximum simulation time
     * @param start The log to start with
     * @param limit The number of logs to get.
     * @return A response (200) with the application logs for the specified execution.
     * @throws WebAppException If the application logs cannot be retrieved.
     */
    @GET
    @Path("/{executionId}/logs")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getApplicationLogs(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId, @QueryParam("deviceList") final List<Long> deviceList,
            @QueryParam("applicationList") final List<String> applicationList, @QueryParam("keyList") final List<String> keyList, @QueryParam("minTime") final Double minTime,
            @QueryParam("maxTime") final Double maxTime, @QueryParam("start") final int start, @QueryParam("limit") final int limit) throws WebAppException {

        JsonArrayBuilder logBuilder = Json.createArrayBuilder();
        for (ApplicationLog log : executionManager.getApplicationLogs(user.getId(), compositeId, executionId, deviceList, applicationList, keyList, minTime, maxTime, start, limit)) {
            logBuilder.add(Json.createObjectBuilder()
                    .add("id", log.getId())
                    .add("deviceId", log.getDeviceId())
                    .add("applicationName", log.getApplicationName())
                    .add("applicationKey", log.getApplicationKey())
                    .add("applicationMessage", log.getApplicationMessage())
                    .add("simulationTime", log.getSimulationTime()));
        }

        JsonObject response = Json.createObjectBuilder()
                .add("logs", logBuilder)
                .add("total", executionManager.getApplicationLogCount(user.getId(), compositeId, executionId, deviceList, applicationList, keyList, minTime, maxTime)).build();

        return Response.ok(response.toString()).build();
    }

    /**
     * Returns the application logs as a CSV file for the specified execution. NOTE: this needs to
     * be a POST command in order for it to work. The file will also be separated by ';' not ','.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param deviceList the device IDs of interest
     * @param applicationList the application names of interest
     * @param keyList the message keys of interest
     * @param minTime the minimum simulation time
     * @param maxTime the maximum simulation time
     * @return A response (200) with the CSV file name attached for the specified execution.
     * @throws WebAppException If the application logs CSV file could not be created.
     */
    @POST
    @Path("/{executionId}/logs")
    @Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
    @Produces({ "text/csv" })
    public Response getApplicationLogsCSV(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId, @FormParam("deviceList") final List<Long> deviceList,
            @FormParam("applicationList") final List<String> applicationList, @FormParam("keyList") final List<String> keyList, @FormParam("minTime") final Double minTime,
            @FormParam("maxTime") final Double maxTime) throws WebAppException {

        StringBuilder logBuilder = new StringBuilder("SEP=;\nid;deviceId;applicationName;applicationKey;applicationMessage;simulationTime\n");
        int end = (int)executionManager.getApplicationLogCount(user.getId(), compositeId, executionId, deviceList, applicationList, keyList, minTime, maxTime);
        for (ApplicationLog log : executionManager.getApplicationLogs(user.getId(), compositeId, executionId, deviceList, applicationList, keyList, minTime, maxTime, 0, end)) {

            logBuilder.append(log.getId());
            logBuilder.append(';');
            logBuilder.append(log.getDeviceId());
            logBuilder.append(';');
            logBuilder.append(log.getApplicationName());
            logBuilder.append(';');
            logBuilder.append(log.getApplicationKey());
            logBuilder.append(';');
            logBuilder.append(log.getApplicationMessage());
            logBuilder.append(';');
            logBuilder.append(log.getSimulationTime());
            logBuilder.append('\n');
        }

        try {

            // write the file content
            String fileName = "ApplicationLogs_Composite" + compositeId + "_Execution" + executionId + ".csv";
            File tmpDir = FileUtils.getTempDirectory();
            File file = new File(tmpDir, fileName);
            FileUtils.writeStringToFile(file, logBuilder.toString(), "UTF-8");

            // return the written file
            ResponseBuilder response = Response.ok((Object)file);
            response.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"");
            return response.build();
        }
        catch (IOException e) {

            throw new WebAppException("App Log CSV File Error", "The exported log file may contain incomplete or corrupt data.");
        }
    }

    /**
     * Returns the unique application names from application logs for the specified execution.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param deviceList The list of host devices (MAC addresses) to consider in the search.
     *        Providing a <code>null</code> or empty list will result in the inclusion of all valid
     *        host devices.
     * @return A response (200) with the unique application names from the application logs for the
     *         specified execution.
     * @throws WebAppException If the unique application names cannot be retrieved.
     */
    @GET
    @Path("/{executionId}/logs/applications")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getApplicationLogApplications(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId,
            @QueryParam("deviceList") final List<Long> deviceList) throws WebAppException {

        return Response.ok(executionManager.getApplicationLogApplications(user.getId(), compositeId, executionId, deviceList)).build();
    }

    /**
     * Returns the unique device IDs from application logs for the specified execution.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A response (200) with the unique device IDs from the application logs for the
     *         specified execution.
     * @throws WebAppException If the unique device IDs cannot be retrieved.
     */
    @GET
    @Path("/{executionId}/logs/devices")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getApplicationLogDevices(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId) throws WebAppException {

        return Response.ok(executionManager.getApplicationLogDevices(user.getId(), compositeId, executionId)).build();
    }

    /**
     * Returns the unique keys from application logs for the specified execution.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param deviceList The list of host devices (MAC addresses) to consider in the search.
     *        Providing a <code>null</code> or empty list will result in the inclusion of all valid
     *        host devices.
     * @param applicationList The list of applications to consider in the search. Providing a
     *        <code>null</code> or empty list will result in the inclusion of all valid
     *        applications.
     * @return A response (200) with the unique keys from the application logs for the specified
     *         execution.
     * @throws WebAppException If the unique keys cannot be retrieved.
     */
    @GET
    @Path("/{executionId}/logs/keys")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getApplicationLogKeys(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId,
            @QueryParam("deviceList") final List<Long> deviceList, @QueryParam("applicationList") final List<String> applicationList) throws WebAppException {

        return Response.ok(executionManager.getApplicationLogKeys(user.getId(), compositeId, executionId, deviceList, applicationList)).build();
    }

    /**
     * Returns the commands for the specified execution.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A response (200) with the commands for the specified execution.
     * @throws WebAppException If the commands cannot be retrieved.
     */
    @GET
    @Path("/{executionId}/commands")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getCommands(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId) throws WebAppException {

        return Response.ok(executionManager.getCommands(user.getId(), compositeId, executionId)).build();
    }

    /**
     * Adds a new injection command to the specified execution.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param simulationId The long ID of the simulation.
     * @param laneId The integer ID of the lane.
     * @param speed The double speed (m/s).
     * @return A response (204) with no additional content.
     * @throws WebAppException If a new injection command cannot be added.
     */
    @POST
    @Path("/{executionId}/commands/injectioncommands")
    @Produces(MediaType.APPLICATION_JSON)
    public Response addInjectionCommand(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId, @FormParam("simulationId") final Long simulationId,
            @FormParam("laneId") final Integer laneId, @FormParam("speed") final Double speed) throws WebAppException {

        RestValidator.validate(
                new SimulationIdValidator(simulationId),
                new LaneIdValidator(laneId),
                new SpeedValidator(speed));

        executionManager.addInjectionCommand(user.getId(), compositeId, executionId, simulationId, laneId, speed);
        return Response.noContent().build();
    }

    /**
     * Adds a new lane command to the specified execution.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param simulationId The long ID of the simulation.
     * @param vehicleId The integer ID of the vehicle.
     * @param command The integer command type.
     * @return A response (204) with no additional content.
     * @throws WebAppException If a new lane command cannot be added.
     */
    @POST
    @Path("/{executionId}/commands/lanecommands")
    @Produces(MediaType.APPLICATION_JSON)
    public Response addLaneCommand(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId, @FormParam("simulationId") final Long simulationId,
            @FormParam("vehicleId") final Integer vehicleId, @FormParam("command") final Integer command) throws WebAppException {

        RestValidator.validate(
                new SimulationIdValidator(simulationId),
                new VehicleIdValidator(vehicleId),
                new LaneCommandValidator(command));

        executionManager.addLaneCommand(user.getId(), compositeId, executionId, simulationId, vehicleId, command);
        return Response.noContent().build();
    }

    /**
     * Adds a new signal command to the specified execution.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param simulationId The long ID of the simulation.
     * @param command The integer command type.
     * @param time The double signal time (s).
     * @return A response (204) with no additional content.
     * @throws WebAppException If a new signal command cannot be added.
     */
    @POST
    @Path("/{executionId}/commands/signalcommands")
    @Produces(MediaType.APPLICATION_JSON)
    public Response addSignalCommand(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId, @FormParam("simulationId") final Long simulationId,
            @FormParam("command") final Integer command, @FormParam("time") final Double time) throws WebAppException {

        RestValidator.validate(
                new SimulationIdValidator(simulationId),
                new SignalCommandValidator(command),
                new TimeValidator(time));

        executionManager.addSignalCommand(user.getId(), compositeId, executionId, simulationId, command, time);
        return Response.noContent().build();
    }

    /**
     * Adds a new speed command to the specified execution.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param simulationId The long ID of the simulation.
     * @param vehicleId The integer ID of the vehicle.
     * @param command The integer command type.
     * @param speed The double speed (m/s).
     * @return A response (204) with no additional content.
     * @throws WebAppException If a new speed command cannot be added.
     */
    @POST
    @Path("/{executionId}/commands/speedcommands")
    @Produces(MediaType.APPLICATION_JSON)
    public Response addSpeedCommand(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId, @FormParam("simulationId") final Long simulationId,
            @FormParam("vehicleId") final Integer vehicleId, @FormParam("command") final Integer command, @FormParam("speed") final Double speed) throws WebAppException {

        RestValidator.validate(
                new SimulationIdValidator(simulationId),
                new VehicleIdValidator(vehicleId),
                new SpeedCommandValidator(command),
                new SpeedValidator(speed));

        executionManager.addSpeedCommand(user.getId(), compositeId, executionId, simulationId, vehicleId, command, speed);
        return Response.noContent().build();
    }

    /**
     * Returns the detectors for the specified execution.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A response (200) with the detectors for the specified execution.
     * @throws WebAppException If the detectors cannot be retrieved.
     */
    @GET
    @Path("/{executionId}/detectors")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getDetectors(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId) throws WebAppException {

        return Response.ok(executionManager.getDetectors(user.getId(), compositeId, executionId)).build();
    }

    /**
     * Returns the messages for the specified execution.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A response (200) with the messages for the specified execution.
     * @throws WebAppException If the messages cannot be retrieved.
     */
    @GET
    @Path("/{executionId}/messages")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getMessages(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId)
            throws WebAppException {

        return Response.ok(executionManager.getMessages(user.getId(), compositeId, executionId)).build();
    }

    /**
     * Returns the messages for the specified device.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param deviceMac The MAC address of the device.
     * @return A response (200) with the messages for the specified device.
     * @throws WebAppException If the messages cannot be retrieved.
     */
    @GET
    @Path("/{executionId}/devices/{deviceMac}/messages")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getMessagesByDevice(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId, @PathParam("deviceMac") final Long deviceMac)
            throws WebAppException {

        return Response.ok(executionManager.getMessagesByDevice(user.getId(), compositeId, executionId, deviceMac)).build();
    }

    /**
     * Returns the signal indications for the specified execution.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A response (200) with the signal indications for the specified execution.
     * @throws WebAppException If the signal indications cannot be retrieved.
     */
    @GET
    @Path("/{executionId}/signals")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getSignalIndications(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId) throws WebAppException {

        return Response.ok(executionManager.getSignalIndications(user.getId(), compositeId, executionId)).build();
    }

    /**
     * Returns the standalone devices for the specified execution.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A response (200) with the standalone devices for the specified execution.
     * @throws WebAppException If the standalone devices cannot be retrieved.
     */
    @GET
    @Path("/{executionId}/standalonedevices")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getStandaloneDevices(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId) throws WebAppException {

        return Response.ok(executionManager.getStandaloneDevices(user.getId(), compositeId, executionId)).build();
    }

    /**
     * Returns the vehicles for the specified execution.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A response (200) with the vehicles for the specified execution.
     * @throws WebAppException If the vehicles cannot be retrieved.
     */
    @GET
    @Path("/{executionId}/vehicles")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getVehicles(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId) throws WebAppException {

        return Response.ok(executionManager.getVehicles(user.getId(), compositeId, executionId)).build();
    }

    /**
     * Returns the on board devices for the specified vehicle.
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @param vehicleId The long ID of the vehicle.
     * @return A response (200) with the on board devices for the specified vehicle.
     * @throws WebAppException If the on board devices cannot be retrieved.
     */
    @GET
    @Path("/{executionId}/vehicles/{vehicleId}/devices")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getOnBoardDevices(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId, @PathParam("vehicleId") final Long vehicleId)
            throws WebAppException {

        return Response.ok(executionManager.getOnBoardDevices(user.getId(), compositeId, executionId, vehicleId)).build();
    }

    /**
     * Returns the execution messages for the specified execution
     * 
     * @param compositeId The long ID of the composite.
     * @param executionId The long ID of the execution.
     * @return A response (200) with the execution messages for the specified execution.
     * @throws WebAppException If the execution messages cannot be retrieved.
     */
    @GET
    @Path("/{executionId}/executionmessages")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getExecutionMessages(@PathParam("compositeId") final Long compositeId, @PathParam("executionId") final Long executionId) throws WebAppException {

        return Response.ok(executionManager.getExecutionMessages(user.getId(), compositeId, executionId)).build();
    }
}
