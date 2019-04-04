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
import org.etexascode.webapp.datamodel.device.FixedCellularDevice;
import org.etexascode.webapp.datamodel.device.RseDevice;
import org.etexascode.webapp.ejb.DeviceManager;
import org.etexascode.webapp.exception.WebAppException;
import org.etexascode.webapp.rest.validation.ApplicationIdListValidator;
import org.etexascode.webapp.rest.validation.ApplicationParameterValidator;
import org.etexascode.webapp.rest.validation.DeviceIdListValidator;
import org.etexascode.webapp.rest.validation.DeviceNameValidator;
import org.etexascode.webapp.rest.validation.MacAddressValidator;
import org.etexascode.webapp.rest.validation.RestValidator;
import org.etexascode.webapp.rest.validation.XCoordinateValidator;
import org.etexascode.webapp.rest.validation.YCoordinateValidator;
import org.etexascode.webapp.rest.validation.ZCoordinateValidator;

/**
 * The REST service for device operation requests.
 * 
 * @author emyers
 */
@RequestScoped
@Path("/api/composites/{compositeId}/devices")
@Interceptors({ StringTrimmer.class })
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public class DeviceService {

    /** The current user. */
    @Inject
    private CurrentUser user;

    /** The device profile transaction manager. */
    @Inject
    private DeviceManager deviceManager;

    /**
     * Returns the devices for the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @return A response (200) with the devices for the specified composite.
     * @throws WebAppException If the devices cannot be retrieved.
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public Response getDevices(@PathParam("compositeId") final Long compositeId) throws WebAppException {

        return Response.ok(deviceManager.getDevices(user.getId(), compositeId)).build();
    }

    /**
     * Removes the specified devices from the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceIds The list of IDs of the devices.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the devices cannot be removed.
     */
    @DELETE
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeDevices(@PathParam("compositeId") final Long compositeId, @QueryParam("deviceIds") final List<Long> deviceIds) throws WebAppException {

        RestValidator.validate(new DeviceIdListValidator(deviceIds));
        deviceManager.removeDevices(user.getId(), compositeId, deviceIds);
        return Response.noContent().build();
    }

    /**
     * Returns the specified device.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @return A response (200) with the specified device.
     * @throws WebAppException If the device cannot be retrieved.
     */
    @GET
    @Path("/{deviceId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getDevice(@PathParam("compositeId") final Long compositeId, @PathParam("deviceId") final Long deviceId) throws WebAppException {

        return Response.ok(deviceManager.getDevice(user.getId(), compositeId, deviceId)).build();
    }

    /**
     * Removes the specified device from the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the device cannot be removed.
     */
    @DELETE
    @Path("/{deviceId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeDevice(@PathParam("compositeId") final Long compositeId, @PathParam("deviceId") final Long deviceId) throws WebAppException {

        deviceManager.removeDevice(user.getId(), compositeId, deviceId);
        return Response.noContent().build();
    }

    /**
     * Adds a new fixed cellular device to the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceName The string device name to set.
     * @param macAddress The long MAC address to set.
     * @param x The double x coordinate (cm) to set.
     * @param y The double y coordinate (cm) to set.
     * @param z The double z coordinate (cm) to set.
     * @return A response (201) with the new fixed cellular device that was added.
     * @throws URISyntaxException If a new fixed cellular device cannot be added.
     * @throws WebAppException If a new fixed cellular device cannot be added.
     */
    @POST
    @Path("/fixedcellulardevices")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addFixedCellularDevice(@PathParam("compositeId") final Long compositeId, @FormParam("deviceName") final String deviceName, @FormParam("macAddress") final Long macAddress,
            @FormParam("x") final Double x, @FormParam("y") final Double y, @FormParam("z") final Double z) throws URISyntaxException, WebAppException {

        RestValidator.validate(
                new DeviceNameValidator(deviceName),
                new MacAddressValidator(macAddress),
                new XCoordinateValidator(x),
                new YCoordinateValidator(y),
                new ZCoordinateValidator(z));

        FixedCellularDevice fixedCellularDevice = deviceManager.addFixedCellularDevice(user.getId(), compositeId, deviceName, macAddress, x, y, z);
        URI uri = new URI(String.format("/api/composites/%d/devices/%d", compositeId, fixedCellularDevice.getId()));
        return Response.created(uri).entity(fixedCellularDevice).build();
    }

    /**
     * Updates the specified fixed cellular device.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @param deviceName The string device name to set.
     * @param macAddress The long MAC address to set.
     * @param x The double x coordinate (cm) to set.
     * @param y The double y coordinate (cm) to set.
     * @param z The double z coordinate (cm) to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the fixed cellular device cannot be updated.
     */
    @PUT
    @Path("/fixedcellulardevices/{deviceId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateFixedCellularDevice(@PathParam("compositeId") final Long compositeId, @PathParam("deviceId") final Long deviceId, @FormParam("deviceName") final String deviceName,
            @FormParam("macAddress") final Long macAddress, @FormParam("x") final Double x, @FormParam("y") final Double y, @FormParam("z") final Double z) throws WebAppException {

        RestValidator.validateOptional(
                new DeviceNameValidator(deviceName),
                new MacAddressValidator(macAddress),
                new XCoordinateValidator(x),
                new YCoordinateValidator(y),
                new ZCoordinateValidator(z));

        deviceManager.updateFixedCellularDevice(user.getId(), compositeId, deviceId, deviceName, macAddress, x, y, z);
        return Response.noContent().build();
    }

    /**
     * Adds a new RSE device to the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceName The string device name to set.
     * @param x The double x coordinate (cm) to set.
     * @param y The double y coordinate (cm) to set.
     * @param z The double z coordinate (cm) to set.
     * @return A response (201) with the new RSE device that was added.
     * @throws URISyntaxException If a new RSE device cannot be added.
     * @throws WebAppException If a new RSE device cannot be added.
     */
    @POST
    @Path("/rsedevices")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addRseDevice(@PathParam("compositeId") final Long compositeId, @FormParam("deviceName") final String deviceName, @FormParam("x") final Double x, @FormParam("y") final Double y,
            @FormParam("z") final Double z) throws URISyntaxException, WebAppException {

        RestValidator.validate(
                new DeviceNameValidator(deviceName),
                new XCoordinateValidator(x),
                new YCoordinateValidator(y),
                new ZCoordinateValidator(z));

        RseDevice rseDevice = deviceManager.addRseDevice(user.getId(), compositeId, deviceName, x, y, z);
        URI uri = new URI(String.format("/api/composites/%d/devices/%d", compositeId, rseDevice.getId()));
        return Response.created(uri).entity(rseDevice).build();
    }

    /**
     * Updates the specified RSE device.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @param deviceName The string device name to set.
     * @param x The double x coordinate (cm) to set.
     * @param y The double y coordinate (cm) to set.
     * @param z The double z coordinate (cm) to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the RSE device cannot be updated.
     */
    @PUT
    @Path("/rsedevices/{deviceId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateRseDevice(@PathParam("compositeId") final Long compositeId, @PathParam("deviceId") final Long deviceId, @FormParam("deviceName") final String deviceName,
            @FormParam("x") final Double x, @FormParam("y") final Double y, @FormParam("z") final Double z) throws WebAppException {

        RestValidator.validateOptional(
                new DeviceNameValidator(deviceName),
                new XCoordinateValidator(x),
                new YCoordinateValidator(y),
                new ZCoordinateValidator(z));

        deviceManager.updateRseDevice(user.getId(), compositeId, deviceId, deviceName, x, y, z);
        return Response.noContent().build();
    }

    /**
     * Returns the applications for the specified device.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @return A response (200) with the applications for the specified device.
     * @throws WebAppException If the applications cannot be retrieved.
     */
    @GET
    @Path("/{deviceId}/applications")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getDeviceApplications(@PathParam("compositeId") final Long compositeId, @PathParam("deviceId") final Long deviceId) throws WebAppException {

        return Response.ok(deviceManager.getDeviceApplications(user.getId(), compositeId, deviceId)).build();
    }

    /**
     * Removes the specified applications from the specified device.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @param applicationIds The list of IDs of the applications.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the applications cannot be removed.
     */
    @DELETE
    @Path("/{deviceId}/applications")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeDeviceApplications(@PathParam("compositeId") final Long compositeId, @PathParam("deviceId") final Long deviceId,
            @QueryParam("applicationIds") final List<Long> applicationIds) throws WebAppException {

        RestValidator.validate(new ApplicationIdListValidator(applicationIds));
        deviceManager.removeDeviceApplications(user.getId(), compositeId, deviceId, applicationIds);
        return Response.noContent().build();
    }

    /**
     * Adds a new application to the specified device.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @param applicationProfileId The long ID of the application profile.
     * @return A response (201) with the new application that was added.
     * @throws URISyntaxException If a new application cannot be added.
     * @throws WebAppException If a new application cannot be added.
     */
    @POST
    @Path("/{deviceId}/applications")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addDeviceApplication(@PathParam("compositeId") final Long compositeId, @PathParam("deviceId") final Long deviceId,
            @FormParam("applicationProfileId") @NotNull(message = "A valid application profile ID is required.") final Long applicationProfileId) throws URISyntaxException, WebAppException {

        Application application = deviceManager.addDeviceApplication(user.getId(), compositeId, deviceId, applicationProfileId);
        URI uri = new URI(String.format("/api/composites/%d/devices/%d/applications/%d", compositeId, deviceId, application.getId()));
        return Response.created(uri).entity(application).build();
    }

    /**
     * Returns the specified device application.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @param applicationId The long ID of the application.
     * @return A response (200) with the specified device application.
     * @throws WebAppException If the device application cannot be retrieved.
     */
    @GET
    @Path("/{deviceId}/applications/{applicationId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getDeviceApplication(@PathParam("compositeId") final Long compositeId, @PathParam("deviceId") final Long deviceId, @PathParam("applicationId") final Long applicationId)
            throws WebAppException {

        return Response.ok(deviceManager.getDeviceApplication(user.getId(), compositeId, deviceId, applicationId)).build();
    }

    /**
     * Removes the specified application from the specified device.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @param applicationId The long ID of the application.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the application cannot be removed.
     */
    @DELETE
    @Path("/{deviceId}/applications/{applicationId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeDeviceApplication(@PathParam("compositeId") final Long compositeId, @PathParam("deviceId") final Long deviceId, @PathParam("applicationId") final Long applicationId)
            throws WebAppException {

        deviceManager.removeDeviceApplication(user.getId(), compositeId, deviceId, applicationId);
        return Response.noContent().build();
    }

    /**
     * Updates the specified application parameter.
     * 
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @param applicationId The long ID of the application.
     * @param applicationParameterId The long ID of the application parameter.
     * @param value The string value to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the application parameter cannot be updated.
     */
    @PUT
    @Path("/{deviceId}/applications/{applicationId}/parameters/{applicationParameterId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateDeviceApplicationParameter(@PathParam("compositeId") final Long compositeId, @PathParam("deviceId") final Long deviceId, @PathParam("applicationId") final Long applicationId,
            @PathParam("applicationParameterId") final Long applicationParameterId, @FormParam("value") final String value) throws WebAppException {

        RestValidator.validateOptional(new ApplicationParameterValidator(value));
        deviceManager.updateDeviceApplicationParameter(user.getId(), compositeId, deviceId, applicationId, applicationParameterId, value);
        return Response.noContent().build();
    }
}
