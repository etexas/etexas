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

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.interceptor.Interceptors;
import javax.validation.constraints.NotNull;
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

import org.etexascode.webapp.cdi.CurrentUser;
import org.etexascode.webapp.datamodel.CellTower;
import org.etexascode.webapp.datamodel.Composite;
import org.etexascode.webapp.datamodel.LaneMapping;
import org.etexascode.webapp.datamodel.application.Application;
import org.etexascode.webapp.ejb.CompositeManager;
import org.etexascode.webapp.exception.WebAppException;
import org.etexascode.webapp.rest.validation.ApplicationIdListValidator;
import org.etexascode.webapp.rest.validation.ApplicationParameterValidator;
import org.etexascode.webapp.rest.validation.CellTowerIdListValidator;
import org.etexascode.webapp.rest.validation.CellTowerNoiseValidator;
import org.etexascode.webapp.rest.validation.CellTowerPowerValidator;
import org.etexascode.webapp.rest.validation.CellularDeviceNoiseValidator;
import org.etexascode.webapp.rest.validation.CellularDevicePowerValidator;
import org.etexascode.webapp.rest.validation.CellularProviderValidator;
import org.etexascode.webapp.rest.validation.CommunicationsModelValidator;
import org.etexascode.webapp.rest.validation.CompositeIdListValidator;
import org.etexascode.webapp.rest.validation.CompositeNameValidator;
import org.etexascode.webapp.rest.validation.DownlinkBandwidthValidator;
import org.etexascode.webapp.rest.validation.DownlinkCarrierFrequencyValidator;
import org.etexascode.webapp.rest.validation.GeographicCalculatorValidator;
import org.etexascode.webapp.rest.validation.LaneMappingIdListValidator;
import org.etexascode.webapp.rest.validation.LatitudeValidator;
import org.etexascode.webapp.rest.validation.LongitudeValidator;
import org.etexascode.webapp.rest.validation.PropagationLossModelValidator;
import org.etexascode.webapp.rest.validation.RestValidator;
import org.etexascode.webapp.rest.validation.SourceLaneValidator;
import org.etexascode.webapp.rest.validation.SourceSimulationValidator;
import org.etexascode.webapp.rest.validation.TargetLaneValidator;
import org.etexascode.webapp.rest.validation.TargetSimulationValidator;
import org.etexascode.webapp.rest.validation.UplinkBandwidthValidator;
import org.etexascode.webapp.rest.validation.UplinkCarrierFrequencyValidator;
import org.etexascode.webapp.rest.validation.XCoordinateValidator;
import org.etexascode.webapp.rest.validation.YCoordinateValidator;
import org.etexascode.webapp.rest.validation.ZCoordinateValidator;

/**
 * The REST service for composite operation requests.
 * 
 * @author emyers
 * @author ttevendale
 */
@RequestScoped
@Path("/api/composites")
@Interceptors({ StringTrimmer.class })
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public class CompositeService {

    /** The composite transaction manager. */
    @Inject
    private CompositeManager compositeManager;

    /** The current user. */
    @Inject
    private CurrentUser user;

    /**
     * Returns the composites for the current user.
     * 
     * @return A response (200) with the composites for the current user.
     * @throws WebAppException If the composites cannot be retrieved.
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public Response getComposites() throws WebAppException {

        return Response.ok(compositeManager.getComposites(user.getId())).build();
    }

    /**
     * Removes the specified composites from the current user.
     * 
     * @param compositeIds The list of IDs of the composites.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the composites cannot be removed.
     */
    @DELETE
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeComposites(@QueryParam("compositeIds") final List<Long> compositeIds) throws WebAppException {

        RestValidator.validate(new CompositeIdListValidator(compositeIds));
        compositeManager.removeComposites(user.getId(), compositeIds);
        return Response.noContent().build();
    }

    /**
     * Adds a new composite to the current user.
     * 
     * @param compositeName The string composite name to set.
     * @return A response (201) with the new composite that was added.
     * @throws URISyntaxException If a new composite cannot be added.
     * @throws WebAppException If a new composite cannot be added.
     */
    @POST
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addComposite(@FormParam("compositeName") final String compositeName) throws URISyntaxException, WebAppException {

        RestValidator.validate(new CompositeNameValidator(compositeName));
        Composite composite = compositeManager.addComposite(user.getId(), compositeName);
        URI uri = new URI(String.format("/api/composites/%d", composite.getId()));
        return Response.created(uri).entity(composite).build();
    }

    /**
     * Returns the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @return A response (200) with the specified composite.
     * @throws WebAppException If the composite cannot be retrieved.
     */
    @GET
    @Path("/{compositeId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getComposite(@PathParam("compositeId") final Long compositeId) throws WebAppException {

        return Response.ok(compositeManager.getComposite(user.getId(), compositeId)).build();
    }

    /**
     * Removes the specified composite from the specified user.
     * 
     * @param compositeId The long ID of the composite.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the composite cannot be removed.
     */
    @DELETE
    @Path("/{compositeId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeComposite(@PathParam("compositeId") final Long compositeId) throws WebAppException {

        compositeManager.removeComposite(user.getId(), compositeId);
        return Response.noContent().build();
    }

    /**
     * Adds a copy of the specified composite to the current user.
     * 
     * @param compositeId The long ID of the composite.
     * @param compositeName The string composite name to set.
     * @return A response (201) with the new composite copy that was added.
     * @throws URISyntaxException If a new composite copy cannot be added.
     * @throws WebAppException If a new composite copy cannot be added.
     */
    @POST
    @Path("/{compositeId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response copyComposite(@PathParam("compositeId") final Long compositeId, @FormParam("compositeName") final String compositeName) throws URISyntaxException, WebAppException {

        RestValidator.validate(new CompositeNameValidator(compositeName));
        Composite composite = compositeManager.copyComposite(user.getId(), compositeId, compositeName);
        URI uri = new URI(String.format("/api/composites/%d", composite.getId()));
        return Response.created(uri).entity(composite).build();
    }

    /**
     * Updates the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param compositeName The string composite name to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the composite cannot be updated.
     */
    @PUT
    @Path("/{compositeId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateComposite(@PathParam("compositeId") final Long compositeId, @FormParam("compositeName") final String compositeName) throws WebAppException {

        RestValidator.validateOptional(new CompositeNameValidator(compositeName));
        compositeManager.updateComposite(user.getId(), compositeId, compositeName);
        return Response.noContent().build();
    }

    /**
     * Returns the cell towers for the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @return A response (200) with the cell towers for the specified composite.
     * @throws WebAppException If the cell towers cannot be retrieved.
     */
    @GET
    @Path("/{compositeId}/celltowers")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getCellTowers(@PathParam("compositeId") final Long compositeId) throws WebAppException {

        return Response.ok(compositeManager.getCellTowers(user.getId(), compositeId)).build();
    }

    /**
     * Removes the specified cell towers from the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param cellTowerIds The list of IDs of the cell towers.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the cell towers cannot be removed.
     */
    @DELETE
    @Path("/{compositeId}/celltowers")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeCellTowers(@PathParam("compositeId") final Long compositeId, @QueryParam("cellTowerIds") final List<Long> cellTowerIds) throws WebAppException {

        RestValidator.validate(new CellTowerIdListValidator(cellTowerIds));
        compositeManager.removeCellTowers(user.getId(), compositeId, cellTowerIds);
        return Response.noContent().build();
    }

    /**
     * Adds a new cell tower to the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param provider The string cellular provider to set.
     * @param x The double x coordinate (cm) to set.
     * @param y The double y coordinate (cm) to set.
     * @param z The double z coordinate (cm) to set.
     * @return A response (201) with the new cell tower that was added.
     * @throws URISyntaxException If a new cell tower cannot be added.
     * @throws WebAppException If a new cell tower cannot be added.
     */
    @POST
    @Path("/{compositeId}/celltowers")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addCellTower(@PathParam("compositeId") final Long compositeId, @FormParam("provider") final String provider, @FormParam("x") final Double x, @FormParam("y") final Double y,
            @FormParam("z") final Double z) throws URISyntaxException, WebAppException {

        RestValidator.validate(
                new CellularProviderValidator(provider),
                new XCoordinateValidator(x),
                new YCoordinateValidator(y),
                new ZCoordinateValidator(z));

        CellTower cellTower = compositeManager.addCellTower(user.getId(), compositeId, provider, x, y, z);
        URI uri = new URI(String.format("/api/composites/%d/celltowers/%d", compositeId, cellTower.getId()));
        return Response.created(uri).entity(cellTower).build();
    }

    /**
     * Returns the specified cell tower.
     * 
     * @param compositeId The long ID of the composite.
     * @param cellTowerId The long ID of the cell tower.
     * @return A response (200) with the specified cell tower.
     * @throws WebAppException If the cell tower cannot be retrieved.
     */
    @GET
    @Path("/{compositeId}/celltowers/{cellTowerId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getCellTower(@PathParam("compositeId") final Long compositeId, @PathParam("cellTowerId") final Long cellTowerId) throws WebAppException {

        return Response.ok(compositeManager.getCellTower(user.getId(), compositeId, cellTowerId)).build();
    }

    /**
     * Removes the cell tower with the specified ID.
     * 
     * @param compositeId The long ID of the composite.
     * @param cellTowerId The long ID of the cell tower.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the cell tower cannot be removed.
     */
    @DELETE
    @Path("/{compositeId}/celltowers/{cellTowerId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeCellTower(@PathParam("compositeId") final Long compositeId, @PathParam("cellTowerId") final Long cellTowerId) throws WebAppException {

        compositeManager.removeCellTower(user.getId(), compositeId, cellTowerId);
        return Response.noContent().build();
    }

    /**
     * Updates the cell tower with the specified ID.
     * 
     * @param compositeId The long ID of the composite.
     * @param cellTowerId The long ID of the cell tower.
     * @param provider The string cellular provider to set.
     * @param x The double x coordinate (cm) to set.
     * @param y The double y coordinate (cm) to set.
     * @param z The double z coordinate (cm) to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the cell tower cannot be updated.
     */
    @PUT
    @Path("/{compositeId}/celltowers/{cellTowerId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateCellTower(@PathParam("compositeId") final Long compositeId, @PathParam("cellTowerId") final Long cellTowerId, @FormParam("provider") final String provider,
            @FormParam("x") final Double x, @FormParam("y") final Double y, @FormParam("z") final Double z) throws WebAppException {

        RestValidator.validateOptional(
                new CellularProviderValidator(provider),
                new XCoordinateValidator(x),
                new YCoordinateValidator(y),
                new ZCoordinateValidator(z));

        compositeManager.updateCellTower(user.getId(), compositeId, cellTowerId, provider, x, y, z);
        return Response.noContent().build();
    }

    /**
     * Returns the cellular configuration for the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @return A response (200) with the cellular configuration for the specified composite.
     * @throws WebAppException If the cellular configuration cannot be retrieved.
     */
    @GET
    @Path("/{compositeId}/cellularconfigurations")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getCellularConfiguration(@PathParam("compositeId") final Long compositeId) throws WebAppException {

        return Response.ok(compositeManager.getCellularConfiguration(user.getId(), compositeId)).build();
    }

    /**
     * Updates the cellular configuration for the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param uplinkBandwidth The integer uplink bandwidth (resource blocks) to set.
     * @param downlinkBandwidth The integer downlink bandwidth (resource blocks) to set.
     * @param uplinkCarrierFrequency The integer uplink carrier frequency to set.
     * @param downlinkCarrierFrequency The integer downlink carrier frequency to set.
     * @param cellTowerNoise The cell tower noise (dB) to set.
     * @param cellTowerPower The cell tower power (dBm) to set.
     * @param cellularDeviceNoise The cellular device noise (dB) to set.
     * @param cellularDevicePower The cellular device power (dBm) to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the cellular configuration cannot be updated.
     */
    @PUT
    @Path("/{compositeId}/cellularconfigurations")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateCellularConfiguration(@PathParam("compositeId") final Long compositeId, @FormParam("uplinkBandwidth") final Integer uplinkBandwidth,
            @FormParam("downlinkBandwidth") final Integer downlinkBandwidth, @FormParam("uplinkCarrierFrequency") final Integer uplinkCarrierFrequency,
            @FormParam("downlinkCarrierFrequency") final Integer downlinkCarrierFrequency, @FormParam("cellTowerNoise") final Double cellTowerNoise,
            @FormParam("cellTowerPower") final Double cellTowerPower, @FormParam("cellularDeviceNoise") final Double cellularDeviceNoise,
            @FormParam("cellularDevicePower") final Double cellularDevicePower) throws WebAppException {

        RestValidator.validateOptional(
                new UplinkBandwidthValidator(uplinkBandwidth),
                new DownlinkBandwidthValidator(downlinkBandwidth),
                new UplinkCarrierFrequencyValidator(uplinkCarrierFrequency),
                new DownlinkCarrierFrequencyValidator(downlinkCarrierFrequency),
                new CellTowerNoiseValidator(cellTowerNoise),
                new CellTowerPowerValidator(cellTowerPower),
                new CellularDeviceNoiseValidator(cellularDeviceNoise),
                new CellularDevicePowerValidator(cellularDevicePower));

        compositeManager.updateCellularConfiguration(user.getId(), compositeId, uplinkBandwidth, downlinkBandwidth, uplinkCarrierFrequency, downlinkCarrierFrequency, cellTowerNoise, cellTowerPower,
                cellularDeviceNoise, cellularDevicePower);

        return Response.noContent().build();
    }

    /**
     * Returns the lane mappings for the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @return A response (200) with the lane mappings for the specified composite.
     * @throws WebAppException If the lane mappings cannot be retrieved.
     */
    @GET
    @Path("/{compositeId}/lanemappings")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getLaneMappings(@PathParam("compositeId") final Long compositeId) throws WebAppException {

        return Response.ok(compositeManager.getLaneMappings(user.getId(), compositeId)).build();
    }

    /**
     * Removes the specified lane mappings from the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param laneMappingIds The list of IDs of the lane mappings.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the lane mappings cannot be removed.
     */
    @DELETE
    @Path("/{compositeId}/lanemappings")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeLaneMappings(@PathParam("compositeId") final Long compositeId, @QueryParam("laneMappingIds") final List<Long> laneMappingIds) throws WebAppException {

        RestValidator.validate(new LaneMappingIdListValidator(laneMappingIds));
        compositeManager.removeLaneMappings(user.getId(), compositeId, laneMappingIds);
        return Response.noContent().build();
    }

    /**
     * Adds a new lane mapping to the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param sourceSimulation The long source simulation ID to set.
     * @param sourceLane The integer source lane ID to set.
     * @param targetSimulation The long target simulation ID to set.
     * @param targetLane The integer target lane ID to set.
     * @return A response (201) with the new lane mapping that was added.
     * @throws URISyntaxException If a new lane mapping cannot be added.
     * @throws WebAppException If a new lane mapping cannot be added.
     */
    @POST
    @Path("/{compositeId}/lanemappings")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addLaneMapping(@PathParam("compositeId") final Long compositeId, @FormParam("sourceSimulation") final Long sourceSimulation, @FormParam("sourceLane") final Integer sourceLane,
            @FormParam("targetSimulation") final Long targetSimulation, @FormParam("targetLane") final Integer targetLane) throws URISyntaxException, WebAppException {

        RestValidator.validate(
                new SourceSimulationValidator(sourceSimulation),
                new SourceLaneValidator(sourceLane),
                new TargetSimulationValidator(targetSimulation),
                new TargetLaneValidator(targetLane));

        LaneMapping laneMapping = compositeManager.addLaneMapping(user.getId(), compositeId, sourceSimulation, sourceLane, targetSimulation, targetLane);
        URI uri = new URI(String.format("/api/composites/%d/lanemappings/%d", compositeId, laneMapping.getId()));
        return Response.created(uri).entity(laneMapping).build();
    }

    /**
     * Returns the specified lane mapping.
     * 
     * @param compositeId The long ID of the composite.
     * @param laneMappingId The long ID of the lane mapping.
     * @return A response (200) with the specified lane mapping.
     * @throws WebAppException If the lane mapping cannot be retrieved.
     */
    @GET
    @Path("/{compositeId}/lanemappings/{laneMappingId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getLaneMapping(@PathParam("compositeId") final Long compositeId, @PathParam("laneMappingId") final Long laneMappingId) throws WebAppException {

        return Response.ok(compositeManager.getLaneMapping(user.getId(), compositeId, laneMappingId)).build();
    }

    /**
     * Removes the lane mapping with the specified ID.
     * 
     * @param compositeId The long ID of the composite.
     * @param laneMappingId The long ID of the lane mapping.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the lane mapping cannot be removed.
     */
    @DELETE
    @Path("/{compositeId}/lanemappings/{laneMappingId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeLaneMapping(@PathParam("compositeId") final Long compositeId, @PathParam("laneMappingId") final Long laneMappingId) throws WebAppException {

        compositeManager.removeLaneMapping(user.getId(), compositeId, laneMappingId);
        return Response.noContent().build();
    }

    /**
     * Updates the lane mapping with the specified ID.
     * 
     * @param compositeId The long ID of the composite.
     * @param laneMappingId The long ID of the lane mapping.
     * @param sourceSimulation The long source simulation ID to set.
     * @param sourceLane The integer source lane ID to set.
     * @param targetSimulation The long target simulation ID to set.
     * @param targetLane The integer target lane ID to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the lane mapping cannot be updated.
     */
    @PUT
    @Path("/{compositeId}/lanemappings/{laneMappingId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateLaneMapping(@PathParam("compositeId") final Long compositeId, @PathParam("laneMappingId") final Long laneMappingId,
            @FormParam("sourceSimulation") final Long sourceSimulation, @FormParam("sourceLane") final Integer sourceLane, @FormParam("targetSimulation") final Long targetSimulation,
            @FormParam("targetLane") final Integer targetLane) throws WebAppException {

        RestValidator.validateOptional(
                new SourceSimulationValidator(sourceSimulation),
                new SourceLaneValidator(sourceLane),
                new TargetSimulationValidator(targetSimulation),
                new TargetLaneValidator(targetLane));

        compositeManager.updateLaneMapping(user.getId(), compositeId, laneMappingId, sourceSimulation, sourceLane, targetSimulation, targetLane);
        return Response.noContent().build();
    }

    /**
     * Returns the lanes for the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @return A response (200) with the lanes for the specified composite.
     * @throws WebAppException If the lanes cannot be retrieved.
     */
    @GET
    @Path("/{compositeId}/lanes")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getLanes(@PathParam("compositeId") final Long compositeId) throws WebAppException {

        return Response.ok(compositeManager.getLanes(user.getId(), compositeId)).build();
    }

    /**
     * Returns the option properties for the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @return A response (200) with the option properties for the specified composite.
     * @throws WebAppException If the options properties cannot be retrieved.
     */
    @GET
    @Path("/{compositeId}/options")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getOptions(@PathParam("compositeId") final Long compositeId) throws WebAppException {

        return Response.ok(compositeManager.getCompositeOptions(user.getId(), compositeId)).build();
    }

    /**
     * Updates the option properties for the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param latitude The double latitude (DD) to set.
     * @param longitude The double longitude (DD) to set.
     * @param geographicCalculator The string geographic calculator to set.
     * @param propagationLossModel The string propagation loss model to set.
     * @param communicationsModel The string communications model to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the option properties cannot be updated.
     */
    @PUT
    @Path("/{compositeId}/options")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateOptions(@PathParam("compositeId") final Long compositeId, @FormParam("latitude") final Double latitude, @FormParam("longitude") final Double longitude,
            @FormParam("geographicCalculator") final String geographicCalculator, @FormParam("propagationLossModel") final String propagationLossModel,
            @FormParam("communicationsModel") final String communicationsModel) throws WebAppException {

        RestValidator.validateOptional(
                new LatitudeValidator(latitude),
                new LongitudeValidator(longitude),
                new GeographicCalculatorValidator(geographicCalculator),
                new PropagationLossModelValidator(propagationLossModel),
                new CommunicationsModelValidator(communicationsModel));

        compositeManager.updateCompositeOptions(user.getId(), compositeId, latitude, longitude, geographicCalculator, propagationLossModel, communicationsModel);
        return Response.noContent().build();
    }

    /**
     * Returns the report applications for the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @return A list of report applications for the specified composite.
     * @throws WebAppException If the report applications cannot be retrieved.
     */
    @GET
    @Path("/{compositeId}/reportapplications")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getReportApplications(@PathParam("compositeId") final Long compositeId) throws WebAppException {

        return Response.ok(compositeManager.getReportApplications(user.getId(), compositeId)).build();
    }

    /**
     * Removes the specified report applications from the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param applicationIds The list of IDs of the report applications.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the report applications cannot be removed.
     */
    @DELETE
    @Path("/{compositeId}/reportapplications")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeReportApplications(@PathParam("compositeId") final Long compositeId, @QueryParam("applicationIds") final List<Long> applicationIds) throws WebAppException {

        RestValidator.validate(new ApplicationIdListValidator(applicationIds));
        compositeManager.removeReportApplications(user.getId(), compositeId, applicationIds);
        return Response.noContent().build();
    }

    /**
     * Adds a new report application to the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param applicationProfileId The long ID of the application.
     * @return A response (201) with the new report application that was added.
     * @throws URISyntaxException If a new report application cannot be added.
     * @throws WebAppException If a new report application cannot be added.
     */
    @POST
    @Path("/{compositeId}/reportapplications")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addReportApplication(@PathParam("compositeId") final Long compositeId,
            @FormParam("applicationProfileId") @NotNull(message = "A valid application profile ID is required.") final Long applicationProfileId) throws URISyntaxException, WebAppException {

        Application application = compositeManager.addReportApplication(user.getId(), compositeId, applicationProfileId);
        URI uri = new URI(String.format("/api/composites/%d/reportapplications/%d", compositeId, application.getId()));
        return Response.created(uri).entity(application).build();
    }

    /**
     * Returns the specified report application.
     * 
     * @param compositeId The long ID of the composite.
     * @param applicationId The long ID of the application.
     * @return A response (200) with the specified application.
     * @throws WebAppException If the application cannot be retrieved.
     */
    @GET
    @Path("/{compositeId}/reportapplications/{applicationId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getReportApplication(@PathParam("compositeId") final Long compositeId, @PathParam("applicationId") final Long applicationId) throws WebAppException {

        return Response.ok(compositeManager.getReportApplication(user.getId(), compositeId, applicationId)).build();
    }

    /**
     * Removes the specified report application from the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param applicationId The long ID of the application.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the report application cannot be removed.
     */
    @DELETE
    @Path("/{compositeId}/reportapplications/{applicationId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeReportApplication(@PathParam("compositeId") final Long compositeId, @PathParam("applicationId") final Long applicationId) throws WebAppException {

        compositeManager.removeReportApplication(user.getId(), compositeId, applicationId);
        return Response.noContent().build();
    }

    /**
     * Updates the specified application parameter.
     * 
     * @param compositeId The long ID of the composite.
     * @param applicationId The long ID of the application.
     * @param applicationParameterId The long ID of the application parameter.
     * @param value The string value to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the application parameter cannot be updated.
     */
    @PUT
    @Path("/{compositeId}/reportapplications/{applicationId}/parameters/{applicationParameterId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateReportApplicationParameter(@PathParam("compositeId") final Long compositeId, @PathParam("applicationId") final Long applicationId,
            @PathParam("applicationParameterId") final Long applicationParameterId, @FormParam("value") final String value) throws WebAppException {

        RestValidator.validateOptional(new ApplicationParameterValidator(value));
        compositeManager.updateReportApplicationParameter(user.getId(), compositeId, applicationId, applicationParameterId, value);
        return Response.noContent().build();
    }
}
