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
import org.etexascode.webapp.datamodel.application.Application;
import org.etexascode.webapp.datamodel.device.CellularDeviceProfile;
import org.etexascode.webapp.datamodel.device.ObuDeviceProfile;
import org.etexascode.webapp.ejb.DeviceProfileManager;
import org.etexascode.webapp.exception.WebAppException;
import org.etexascode.webapp.rest.validation.ApplicationIdListValidator;
import org.etexascode.webapp.rest.validation.ApplicationParameterValidator;
import org.etexascode.webapp.rest.validation.DeviceProfileIdListValidator;
import org.etexascode.webapp.rest.validation.DeviceProfileNameValidator;
import org.etexascode.webapp.rest.validation.DeviceProfilePercentageValidator;
import org.etexascode.webapp.rest.validation.MaxDeviceQuantityValidator;
import org.etexascode.webapp.rest.validation.MinDeviceQuantityValidator;
import org.etexascode.webapp.rest.validation.RestValidator;

/**
 * The REST service for device profile operation requests.
 * 
 * @author emyers
 */
@RequestScoped
@Path("/api/composites/{compositeId}/deviceprofiles")
@Interceptors({ StringTrimmer.class })
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public class DeviceProfileService {

    /** The current user. */
    @Inject
    private CurrentUser user;

    /** The device profile transaction manager. */
    @Inject
    private DeviceProfileManager deviceProfileManager;

    /**
     * Returns the device profiles for the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @return A response (200) with the device profiles for the specified composite.
     * @throws WebAppException If the device profiles cannot be retrieved.
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public Response getDeviceProfiles(@PathParam("compositeId") final Long compositeId) throws WebAppException {

        return Response.ok(deviceProfileManager.getDeviceProfiles(user.getId(), compositeId)).build();
    }

    /**
     * Removes the specified device profiles from the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceProfileIds The list of IDs of the device profiles.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the device profiles cannot be removed.
     */
    @DELETE
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeDeviceProfiles(@PathParam("compositeId") final Long compositeId, @QueryParam("deviceProfileIds") final List<Long> deviceProfileIds) throws WebAppException {

        RestValidator.validate(new DeviceProfileIdListValidator(deviceProfileIds));
        deviceProfileManager.removeDeviceProfiles(user.getId(), compositeId, deviceProfileIds);
        return Response.noContent().build();
    }

    /**
     * Returns the specified device profile.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @return A response (200) with the specified device profile.
     * @throws WebAppException If the device profile cannot be retrieved.
     */
    @GET
    @Path("/{deviceProfileId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getDeviceProfile(@PathParam("compositeId") final Long compositeId, @PathParam("deviceProfileId") final Long deviceProfileId) throws WebAppException {

        return Response.ok(deviceProfileManager.getDeviceProfile(user.getId(), compositeId, deviceProfileId)).build();
    }

    /**
     * Removes the specified device profile from the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the device profile cannot be removed.
     */
    @DELETE
    @Path("/{deviceProfileId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeDeviceProfile(@PathParam("compositeId") final Long compositeId, @PathParam("deviceProfileId") final Long deviceProfileId) throws WebAppException {

        deviceProfileManager.removeDeviceProfile(user.getId(), compositeId, deviceProfileId);
        return Response.noContent().build();
    }

    /**
     * Adds a new cellular device profile to the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceProfileName The string device profile name to set.
     * @param minDevices The integer minimum number of devices per affected vehicle to set.
     * @param maxDevices The integer maximum number of devices per affected vehicle to set.
     * @param percentage The double percentage of affected vehicles to set.
     * @return A response (201) with the new cellular device profile that was added.
     * @throws URISyntaxException If a new cellular device profile cannot be added.
     * @throws WebAppException If a new cellular device profile cannot be added.
     */
    @POST
    @Path("/cellulardeviceprofiles")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addCellularDeviceProfile(@PathParam("compositeId") final Long compositeId, @FormParam("deviceProfileName") final String deviceProfileName,
            @FormParam("minDevices") final Integer minDevices, @FormParam("maxDevices") final Integer maxDevices, @FormParam("percentage") final Double percentage)
            throws URISyntaxException, WebAppException {

        RestValidator.validate(
                new DeviceProfileNameValidator(deviceProfileName),
                new MinDeviceQuantityValidator(minDevices),
                new MaxDeviceQuantityValidator(maxDevices),
                new DeviceProfilePercentageValidator(percentage));

        CellularDeviceProfile cellularDeviceProfile = deviceProfileManager.addCellularDeviceProfile(user.getId(), compositeId, deviceProfileName, minDevices, maxDevices, percentage);
        URI uri = new URI(String.format("/api/composites/%d/deviceprofiles/%d", compositeId, cellularDeviceProfile.getId()));
        return Response.created(uri).entity(cellularDeviceProfile).build();
    }

    /**
     * Updates the specified cellular device profile.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @param deviceProfileName The string device profile name to set.
     * @param minDevices The integer minimum number of devices per affected vehicle to set.
     * @param maxDevices The integer maximum number of devices per affected vehicle to set.
     * @param percentage The double percentage of affected vehicles to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the cellular device profile cannot be updated.
     */
    @PUT
    @Path("/cellulardeviceprofiles/{deviceProfileId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updatedCellularDeviceProfile(@PathParam("compositeId") final Long compositeId, @PathParam("deviceProfileId") final Long deviceProfileId,
            @FormParam("deviceProfileName") final String deviceProfileName, @FormParam("minDevices") final Integer minDevices, @FormParam("maxDevices") final Integer maxDevices,
            @FormParam("percentage") final Double percentage) throws WebAppException {

        RestValidator.validateOptional(
                new DeviceProfileNameValidator(deviceProfileName),
                new MinDeviceQuantityValidator(minDevices),
                new MaxDeviceQuantityValidator(maxDevices),
                new DeviceProfilePercentageValidator(percentage));

        deviceProfileManager.updateCellularDeviceProfile(user.getId(), compositeId, deviceProfileId, deviceProfileName, minDevices, maxDevices, percentage);
        return Response.noContent().build();
    }

    /**
     * Adds a new OBU device profile to the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceProfileName The string device profile name to set.
     * @param percentage The double percentage of affected vehicles to set.
     * @return A response (201) with the new OBU device profile that was added.
     * @throws URISyntaxException If a new OBU device profile cannot be added.
     * @throws WebAppException If a new OBU device profile cannot be added.
     */
    @POST
    @Path("/obudeviceprofiles")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addObuDeviceProfile(@PathParam("compositeId") final Long compositeId, @FormParam("deviceProfileName") final String deviceProfileName,
            @FormParam("percentage") final Double percentage) throws URISyntaxException, WebAppException {

        RestValidator.validate(
                new DeviceProfileNameValidator(deviceProfileName),
                new DeviceProfilePercentageValidator(percentage));

        ObuDeviceProfile obuDeviceProfile = deviceProfileManager.addObuDeviceProfile(user.getId(), compositeId, deviceProfileName, percentage);
        URI uri = new URI(String.format("/api/composites/%d/deviceprofiles/%d", compositeId, obuDeviceProfile.getId()));
        return Response.created(uri).entity(obuDeviceProfile).build();
    }

    /**
     * Updates the specified OBU device profile.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @param deviceProfileName The string device profile name to set.
     * @param percentage The double percentage of affected vehicles to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the OBU device profile cannot be updated.
     */
    @PUT
    @Path("/obudeviceprofiles/{deviceProfileId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateObuDeviceProfile(@PathParam("compositeId") final Long compositeId, @PathParam("deviceProfileId") final Long deviceProfileId,
            @FormParam("deviceProfileName") final String deviceProfileName, @FormParam("percentage") final Double percentage) throws WebAppException {

        RestValidator.validateOptional(
                new DeviceProfileNameValidator(deviceProfileName),
                new DeviceProfilePercentageValidator(percentage));

        deviceProfileManager.updateObuDeviceProfile(user.getId(), compositeId, deviceProfileId, deviceProfileName, percentage);
        return Response.noContent().build();
    }

    /**
     * Returns the applications for the specified device profile.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @return A response (200) with the applications for the specified device profile.
     * @throws WebAppException If the applications cannot be retrieved.
     */
    @GET
    @Path("/{deviceProfileId}/applications")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getDeviceProfileApplications(@PathParam("compositeId") final Long compositeId, @PathParam("deviceProfileId") final Long deviceProfileId) throws WebAppException {

        return Response.ok(deviceProfileManager.getDeviceProfileApplications(user.getId(), compositeId, deviceProfileId)).build();
    }

    /**
     * Removes the specified applications from the specified device profile.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @param applicationIds The list of IDs of the applications.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the applications cannot be removed.
     */
    @DELETE
    @Path("/{deviceProfileId}/applications")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeDeviceProfileApplications(@PathParam("compositeId") final Long compositeId, @PathParam("deviceProfileId") final Long deviceProfileId,
            @QueryParam("applicationIds") final List<Long> applicationIds) throws WebAppException {

        RestValidator.validate(new ApplicationIdListValidator(applicationIds));
        deviceProfileManager.removeDeviceProfileApplications(user.getId(), compositeId, deviceProfileId, applicationIds);
        return Response.noContent().build();
    }

    /**
     * Adds a new application to the specified device profile.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @param applicationProfileId The long ID of the application profile.
     * @return A response (201) with the new application that was added.
     * @throws URISyntaxException If a new application cannot be added.
     * @throws WebAppException If a new application cannot be added.
     */
    @POST
    @Path("/{deviceProfileId}/applications")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addDeviceProfileApplication(@PathParam("compositeId") final Long compositeId, @PathParam("deviceProfileId") final Long deviceProfileId,
            @FormParam("applicationProfileId") @NotNull(message = "A valid application profile ID is required.") final Long applicationProfileId) throws URISyntaxException, WebAppException {

        Application application = deviceProfileManager.addDeviceProfileApplication(user.getId(), compositeId, deviceProfileId, applicationProfileId);
        URI uri = new URI(String.format("/api/composites/%d/deviceprofiles/%d/applications/%d", compositeId, deviceProfileId, application.getId()));
        return Response.created(uri).entity(application).build();
    }

    /**
     * Returns the specified device profile application.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @param applicationId The long ID of the application.
     * @return A response (200) with the specified device profile application.
     * @throws WebAppException If the device profile application cannot be retrieved.
     */
    @GET
    @Path("/{deviceProfileId}/applications/{applicationId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getDeviceProfileApplication(@PathParam("compositeId") final Long compositeId, @PathParam("deviceProfileId") final Long deviceProfileId,
            @PathParam("applicationId") final Long applicationId) throws WebAppException {

        return Response.ok(deviceProfileManager.getDeviceProfileApplication(user.getId(), compositeId, deviceProfileId, applicationId)).build();
    }

    /**
     * Removes the specified application from the specified device profile.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @param applicationId The long ID of the application.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the application cannot be removed.
     */
    @DELETE
    @Path("/{deviceProfileId}/applications/{applicationId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeDeviceProfileApplication(@PathParam("compositeId") final Long compositeId, @PathParam("deviceProfileId") final Long deviceProfileId,
            @PathParam("applicationId") final Long applicationId) throws WebAppException {

        deviceProfileManager.removeDeviceProfileApplication(user.getId(), compositeId, deviceProfileId, applicationId);
        return Response.noContent().build();
    }

    /**
     * Updates the specified application parameter.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @param applicationId The long ID of the application.
     * @param applicationParameterId The long ID of the application parameter.
     * @param value The string value to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the application parameter cannot be updated.
     */
    @PUT
    @Path("/{deviceProfileId}/applications/{applicationId}/parameters/{applicationParameterId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateDeviceProfileApplicationParameter(@PathParam("compositeId") final Long compositeId, @PathParam("deviceProfileId") final Long deviceProfileId,
            @PathParam("applicationId") final Long applicationId, @PathParam("applicationParameterId") final Long applicationParameterId, @FormParam("value") final String value)
            throws WebAppException {

        RestValidator.validateOptional(new ApplicationParameterValidator(value));
        deviceProfileManager.updateDeviceProfileApplicationParameter(user.getId(), compositeId, deviceProfileId, applicationId, applicationParameterId, value);
        return Response.noContent().build();
    }
}
