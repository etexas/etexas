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
package org.etexascode.webapp.rest;

import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.util.List;

import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.interceptor.Interceptors;
import javax.servlet.ServletContext;
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
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.etexascode.webapp.cdi.CurrentUser;
import org.etexascode.webapp.datamodel.Detector;
import org.etexascode.webapp.datamodel.Simulation;
import org.etexascode.webapp.ejb.SimulationManager;
import org.etexascode.webapp.exception.WebAppException;
import org.etexascode.webapp.rest.validation.DetectorDistanceValidator;
import org.etexascode.webapp.rest.validation.DetectorHeightValidator;
import org.etexascode.webapp.rest.validation.DetectorIdListValidator;
import org.etexascode.webapp.rest.validation.DetectorWidthValidator;
import org.etexascode.webapp.rest.validation.RestValidator;
import org.etexascode.webapp.rest.validation.SimulationIdListValidator;
import org.etexascode.webapp.rest.validation.SimulationNameValidator;
import org.etexascode.webapp.rest.validation.TargetCompositeValidator;
import org.etexascode.webapp.rest.validation.TargetLaneValidator;
import org.etexascode.webapp.rest.validation.TemplateNameValidator;
import org.etexascode.webapp.rest.validation.XCoordinateValidator;
import org.etexascode.webapp.rest.validation.YCoordinateValidator;

/**
 * The REST service for simulation operation requests.
 * 
 * @author bbadillo
 * @author jrutherford
 * @author keagan
 * @author emyers
 * @author ttevendale
 */
@RequestScoped
@Path("/api/composites/{compositeId}/simulations")
@Interceptors({ StringTrimmer.class })
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public class SimulationService {

    /** The current user. */
    @Inject
    private CurrentUser user;

    /** The servlet context. */
    @Context
    private ServletContext servletContext;

    /** The simulation transaction manager. */
    @Inject
    private SimulationManager simulationManager;

    /**
     * Returns the simulations for the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @return A response (200) with the simulations for the specified composite.
     * @throws WebAppException If the simulations cannot be retrieved.
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public Response getSimulations(@PathParam("compositeId") final Long compositeId) throws WebAppException {

        return Response.ok(simulationManager.getSimulations(user.getId(), compositeId)).build();
    }

    /**
     * Removes the specified simulations from the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param simulationIds The list of IDs of the simulations.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the simulations cannot be removed.
     */
    @DELETE
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeSimulations(@PathParam("compositeId") final Long compositeId, @QueryParam("simulationIds") final List<Long> simulationIds) throws WebAppException {

        RestValidator.validate(new SimulationIdListValidator(simulationIds));
        simulationManager.removeSimulations(user.getId(), compositeId, simulationIds);
        return Response.noContent().build();
    }

    /**
     * Adds a new simulation to the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param simulationName The string simulation name to set.
     * @param x The x coordinate of the simulation to set.
     * @param y The y coordinate of the simulation to set.
     * @param templateName The string template name to build.
     * @return A response (201) with the new simulation that was added.
     * @throws URISyntaxException If a new simulation cannot be added.
     * @throws WebAppException If a new simulation cannot be added.
     */
    @POST
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addSimulation(@PathParam("compositeId") final Long compositeId, @FormParam("simulationName") final String simulationName, @FormParam("x") final Double x,
            @FormParam("y") final Double y, @FormParam("templateName") final String templateName) throws URISyntaxException, WebAppException {

        RestValidator.validate(
                new SimulationNameValidator(simulationName),
                new XCoordinateValidator(x),
                new YCoordinateValidator(y),
                new TemplateNameValidator(templateName));

        String templatePath = servletContext.getRealPath(String.format("/TEXAS_Projects/%s.zip", templateName));
        Simulation simulation = simulationManager.addSimulation(user.getId(), compositeId, simulationName, x, y, Paths.get(templatePath));
        URI uri = new URI(String.format("/api/composites/%d/simulations/%d", compositeId, simulation.getId()));
        return Response.created(uri).entity(simulation).build();
    }

    /**
     * Returns the specified simulation.
     * 
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @return A response (200) with the specified simulation.
     * @throws WebAppException If the simulation cannot be retrieved.
     */
    @GET
    @Path("/{simulationId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getSimulation(@PathParam("compositeId") final Long compositeId, @PathParam("simulationId") final Long simulationId) throws WebAppException {

        return Response.ok(simulationManager.getSimulation(user.getId(), compositeId, simulationId)).build();
    }

    /**
     * Removes the specified simulation from the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the simulation cannot be removed.
     */
    @DELETE
    @Path("/{simulationId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeSimulation(@PathParam("compositeId") final Long compositeId, @PathParam("simulationId") final Long simulationId) throws WebAppException {

        simulationManager.removeSimulation(user.getId(), compositeId, simulationId);
        return Response.noContent().build();
    }

    /**
     * Adds a copy of the specified simulation to the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param targetCompositeId The long ID of the target composite.
     * @param simulationName The string simulation name to set.
     * @param x The x coordinate of the simulation to set.
     * @param y The y coordinate of the simulation to set.
     * @return A response (201) with the new simulation copy that was added.
     * @throws URISyntaxException If a new simulation copy cannot be added.
     * @throws WebAppException If a new simulation copy cannot be added.
     */
    @POST
    @Path("/{simulationId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response copySimulation(@PathParam("compositeId") final Long compositeId, @PathParam("simulationId") final Long simulationId, @FormParam("targetCompositeId") final Long targetCompositeId,
            @FormParam("simulationName") final String simulationName, @FormParam("x") final Double x, @FormParam("y") final Double y) throws URISyntaxException, WebAppException {

        RestValidator.validate(
                new TargetCompositeValidator(targetCompositeId),
                new SimulationNameValidator(simulationName),
                new XCoordinateValidator(x),
                new YCoordinateValidator(y));

        Simulation simulation = simulationManager.copySimulation(user.getId(), compositeId, simulationId, targetCompositeId, simulationName, x, y);
        URI uri = new URI(String.format("/api/composites/%d/simulations/%d", compositeId, simulation.getId()));
        return Response.created(uri).entity(simulation).build();
    }

    /**
     * Updates the specified simulation.
     * 
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param simulationName The string simulation name to set.
     * @param x The x coordinate of the simulation to set.
     * @param y The y coordinate of the simulation to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the simulation cannot be updated.
     */
    @PUT
    @Path("/{simulationId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateSimulation(@PathParam("compositeId") final Long compositeId, @PathParam("simulationId") final Long simulationId, @FormParam("simulationName") final String simulationName,
            @FormParam("x") final Double x, @FormParam("y") final Double y) throws WebAppException {

        RestValidator.validateOptional(
                new SimulationNameValidator(simulationName),
                new XCoordinateValidator(x),
                new YCoordinateValidator(y));

        simulationManager.updateSimulation(user.getId(), compositeId, simulationId, simulationName, x, y);
        return Response.noContent().build();
    }

    /**
     * Returns the detectors for the specified simulation.
     * 
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @return A response (200) with the detectors for the specified simulation.
     * @throws WebAppException If the detectors cannot be retrieved.
     */
    @GET
    @Path("/{simulationId}/detectors")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getDetectors(@PathParam("compositeId") final Long compositeId, @PathParam("simulationId") final Long simulationId) throws WebAppException {

        return Response.ok(simulationManager.getDetectors(user.getId(), compositeId, simulationId)).build();
    }

    /**
     * Removes the specified detectors from the specified simulation.
     * 
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param detectorIds The list of IDs of the detectors.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the detectors cannot be removed.
     */
    @DELETE
    @Path("/{simulationId}/detectors")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeDetectors(@PathParam("compositeId") final Long compositeId, @PathParam("simulationId") final Long simulationId, @QueryParam("detectorIds") final List<Long> detectorIds)
            throws WebAppException {

        RestValidator.validate(new DetectorIdListValidator(detectorIds));
        simulationManager.removeDetectors(user.getId(), compositeId, simulationId, detectorIds);
        return Response.noContent().build();
    }

    /**
     * Adds a new detector to the specified simulation.
     * 
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param lane The integer lane number to set.
     * @param width The double detector width (cm) to set.
     * @param height The double detector height (cm) to set.
     * @param distance The double distance from the stop line (cm) to set.
     * @return A response (201) with the new detector that was added.
     * @throws URISyntaxException If a new detector cannot be added.
     * @throws WebAppException If a new detector cannot be added.
     */
    @POST
    @Path("/{simulationId}/detectors")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addDetector(@PathParam("compositeId") final Long compositeId, @PathParam("simulationId") final Long simulationId, @FormParam("lane") final Integer lane,
            @FormParam("width") final Double width, @FormParam("height") final Double height, @FormParam("distance") final Double distance) throws URISyntaxException, WebAppException {

        RestValidator.validate(
                new TargetLaneValidator(lane),
                new DetectorWidthValidator(width),
                new DetectorHeightValidator(height),
                new DetectorDistanceValidator(distance));

        Detector detector = simulationManager.addDetector(user.getId(), compositeId, simulationId, lane, width, height, distance);
        URI uri = new URI(String.format("/api/composites/%d/simulations/%d/detectors/%d", compositeId, simulationId, detector.getId()));
        return Response.created(uri).entity(detector).build();
    }

    /**
     * Returns the specified detector.
     * 
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param detectorId The long ID of the detector.
     * @return A response (200) with the specified detector.
     * @throws WebAppException If the detector cannot be retrieved.
     */
    @GET
    @Path("/{simulationId}/detectors/{detectorId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getDetector(@PathParam("compositeId") final Long compositeId, @PathParam("simulationId") final Long simulationId, @PathParam("detectorId") final Long detectorId)
            throws WebAppException {

        return Response.ok(simulationManager.getDetector(user.getId(), compositeId, simulationId, detectorId)).build();
    }

    /**
     * Removes the specified detector from the specified simulation.
     * 
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param detectorId The long ID of the detector.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the detector cannot be removed.
     */
    @DELETE
    @Path("/{simulationId}/detectors/{detectorId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeDetector(@PathParam("compositeId") final Long compositeId, @PathParam("simulationId") final Long simulationId, @PathParam("detectorId") final Long detectorId)
            throws WebAppException {

        simulationManager.removeDetector(user.getId(), compositeId, simulationId, detectorId);
        return Response.noContent().build();
    }

    /**
     * Updates the specified detector.
     * 
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param detectorId The long ID of the detector.
     * @param lane The integer lane number to set.
     * @param width The double detector width (cm) to set.
     * @param height The double detector height (cm) to set.
     * @param distance The double distance from the stop line (cm) to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the detector cannot be updated.
     */
    @PUT
    @Path("/{simulationId}/detectors/{detectorId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateDetector(@PathParam("compositeId") final Long compositeId, @PathParam("simulationId") final Long simulationId, @PathParam("detectorId") final Long detectorId,
            @FormParam("lane") final Integer lane, @FormParam("width") final Double width, @FormParam("height") final Double height, @FormParam("distance") final Double distance)
            throws WebAppException {

        RestValidator.validateOptional(
                new TargetLaneValidator(lane),
                new DetectorWidthValidator(width),
                new DetectorHeightValidator(height),
                new DetectorDistanceValidator(distance));

        simulationManager.updateDetector(user.getId(), compositeId, simulationId, detectorId, lane, width, height, distance);
        return Response.noContent().build();
    }

    /**
     * Returns the lanes for the specified simulation.
     * 
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @return A response (200) with the lanes for the specified simulation.
     * @throws WebAppException If the lanes cannot be retrieved.
     */
    @GET
    @Path("/{simulationId}/lanes")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getLanes(@PathParam("compositeId") final Long compositeId, @PathParam("simulationId") final Long simulationId) throws WebAppException {

        return Response.ok(simulationManager.getLanes(user.getId(), compositeId, simulationId)).build();
    }
}