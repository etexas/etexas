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
import org.etexascode.webapp.datamodel.topography.Building;
import org.etexascode.webapp.ejb.TopographyManager;
import org.etexascode.webapp.exception.WebAppException;
import org.etexascode.webapp.rest.validation.BuildingHeightValidator;
import org.etexascode.webapp.rest.validation.BuildingLengthValidator;
import org.etexascode.webapp.rest.validation.BuildingNameValidator;
import org.etexascode.webapp.rest.validation.BuildingWidthValidator;
import org.etexascode.webapp.rest.validation.FeatureIdListValidator;
import org.etexascode.webapp.rest.validation.RestValidator;
import org.etexascode.webapp.rest.validation.XCoordinateValidator;
import org.etexascode.webapp.rest.validation.YCoordinateValidator;

/**
 * The REST service for topography operation requests.
 * 
 * @author ttevendale
 */
@RequestScoped
@Path("/api/composites/{compositeId}/topography")
@Interceptors({ StringTrimmer.class })
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public class TopographyService {

    /** The current user. */
    @Inject
    private CurrentUser user;

    /** The topography transaction manager. */
    @Inject
    private TopographyManager topographyManager;

    /**
     * Returns the topography features for the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @return A response (200) with the topography for the specified composite.
     * @throws WebAppException If the topography cannot be retrieved.
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public Response getTopographyFeatures(@PathParam("compositeId") final Long compositeId) throws WebAppException {

        return Response.ok(topographyManager.getTopographyFeatures(user.getId(), compositeId)).build();
    }

    /**
     * Removes the specified topography features from the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param featureIds The list of IDs of the topography features.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the topography features cannot be removed.
     */
    @DELETE
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeTopographyFeatures(@PathParam("compositeId") final Long compositeId, @QueryParam("featureIds") final List<Long> featureIds) throws WebAppException {

        RestValidator.validate(new FeatureIdListValidator(featureIds));
        topographyManager.removeTopographyFeatures(user.getId(), compositeId, featureIds);
        return Response.noContent().build();
    }

    /**
     * Adds a new building to the specified compsite.
     * 
     * @param compositeId The long ID of the composite.
     * @param featureName The string feature name to set.
     * @param x The double x coordinate (cm) to set.
     * @param y The double y coordinate (cm) to set.
     * @param width The double width (cm) to set.
     * @param length The double length (cm) to set.
     * @param height The double height (cm) to set.
     * @return A response (201) with the new building that was added.
     * @throws URISyntaxException If a new building cannot be added.
     * @throws WebAppException If a new building cannot be added.
     */
    @POST
    @Path("/buildings")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addBuilding(@PathParam("compositeId") final Long compositeId, @FormParam("featureName") final String featureName, @FormParam("x") final Double x, @FormParam("y") final Double y,
            @FormParam("width") final Double width, @FormParam("length") final Double length, @FormParam("height") final Double height) throws URISyntaxException, WebAppException {

        RestValidator.validate(
                new BuildingNameValidator(featureName),
                new XCoordinateValidator(x),
                new YCoordinateValidator(y),
                new BuildingWidthValidator(width),
                new BuildingLengthValidator(length),
                new BuildingHeightValidator(height));

        Building building = topographyManager.addBuilding(user.getId(), compositeId, featureName, x, y, width, length, height);
        URI uri = new URI(String.format("/api/composites/%d/topography/%d", compositeId, building.getId()));
        return Response.created(uri).entity(building).build();
    }

    /**
     * Updates the specified building.
     * 
     * @param compositeId The long ID of the composite.
     * @param featureId The long ID of the topography feature.
     * @param featureName The string feature name to set.
     * @param x The double x coordinate (cm) to set.
     * @param y The double y coordinate (cm) to set.
     * @param width The double width (cm) to set.
     * @param length The double length (cm) to set.
     * @param height The double height (cm) to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the building cannot be updated.
     */
    @PUT
    @Path("/buildings/{featureId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateBuilding(@PathParam("compositeId") final Long compositeId, @PathParam("featureId") final Long featureId, @FormParam("featureName") final String featureName,
            @FormParam("x") final Double x, @FormParam("y") final Double y, @FormParam("width") final Double width, @FormParam("length") final Double length, @FormParam("height") final Double height)
            throws WebAppException {

        RestValidator.validateOptional(
                new BuildingNameValidator(featureName),
                new XCoordinateValidator(x),
                new YCoordinateValidator(y),
                new BuildingWidthValidator(width),
                new BuildingLengthValidator(length),
                new BuildingHeightValidator(height));

        topographyManager.updateBuilding(user.getId(), compositeId, featureId, featureName, x, y, width, length, height);
        return Response.noContent().build();
    }

    /**
     * Returns the specified topography feature.
     * 
     * @param compositeId The long ID of the composite.
     * @param featureId The long ID of the feature.
     * @return A response (200) with the specified feature.
     * @throws WebAppException If the feature cannot be retrieved.
     */
    @GET
    @Path("/{featureId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getTopographyFeature(@PathParam("compositeId") final Long compositeId, @PathParam("featureId") final Long featureId) throws WebAppException {

        return Response.ok(topographyManager.getTopographyFeature(user.getId(), compositeId, featureId)).build();
    }

    /**
     * Removes the specified topography feature from the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param featureId The long ID of the topography feature.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the topography feature cannot be removed.
     */
    @DELETE
    @Path("/{featureId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeTopographyFeature(@PathParam("compositeId") final Long compositeId, @PathParam("featureId") final Long featureId)
            throws WebAppException {

        topographyManager.removeTopographyFeature(user.getId(), compositeId, featureId);
        return Response.noContent().build();
    }

}
