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

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.util.List;
import java.util.ResourceBundle;

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
import org.etexascode.webapp.datamodel.application.ApplicationProfile;
import org.etexascode.webapp.datamodel.application.NativeApplicationProfile;
import org.etexascode.webapp.datamodel.application.RemoteApplicationProfile;
import org.etexascode.webapp.ejb.ApplicationProcessor;
import org.etexascode.webapp.ejb.ApplicationProfileManager;
import org.etexascode.webapp.exception.WebAppException;
import org.etexascode.webapp.rest.validation.ApplicationProfileFileNameValidator;
import org.etexascode.webapp.rest.validation.ApplicationProfileIdListValidator;
import org.etexascode.webapp.rest.validation.ApplicationProfileNameValidator;
import org.etexascode.webapp.rest.validation.CommandLineValidator;
import org.etexascode.webapp.rest.validation.DeviceTypeValidator;
import org.etexascode.webapp.rest.validation.HostAddressValidator;
import org.etexascode.webapp.rest.validation.PortNumberValidator;
import org.etexascode.webapp.rest.validation.RestValidator;

/**
 * The REST service for application operation requests.
 * 
 * @author bbadillo
 * @author jrutherford
 * @author keagan
 * @author emyers
 * @author ttevendale
 */
@RequestScoped
@Path("/api/applicationprofiles")
@Interceptors({ StringTrimmer.class })
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public class ApplicationProfileService {

    /** The embedded application profile file names. */
    private static final String[] EMBEDDED_APPLICATION_PROFILES = { "j2735", "intellifusion", "report" };

    /** The application transaction manager. */
    @Inject
    private ApplicationProfileManager applicationProfileManager;

    /** The current user. */
    @Inject
    private CurrentUser user;

    /** The servlet context. */
    @Context
    private ServletContext servletContext;

    /**
     * Returns the application profiles for the current user.
     * 
     * @return A response (200) with the application profiles for the current user.
     * @throws WebAppException If the application profiles cannot be retrieved.
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public Response getApplicationProfiles() throws WebAppException {

        List<ApplicationProfile> applications = applicationProfileManager.getApplicationProfiles(user.getId());

        if (applications.isEmpty()) {

            for (String fileName : ApplicationProfileService.EMBEDDED_APPLICATION_PROFILES) {

                String filePath = servletContext.getRealPath(ResourceBundle.getBundle("version").getString(fileName));

                try {

                    byte[] fileData = ApplicationProcessor.extractJarData(Paths.get(filePath));
                    applicationProfileManager.addJarApplicationProfiles(user.getId(), fileName, fileData, true);
                }
                catch (IOException exception) {

                    throw new WebAppException("Get Application Profiles Failure", "Embedded application profiles could not be loaded.", Response.Status.INTERNAL_SERVER_ERROR);
                }
            }

            applications = applicationProfileManager.getApplicationProfiles(user.getId());
        }

        return Response.ok(applications).build();
    }

    /**
     * Removes the specified application profiles from the current user.
     * 
     * @param applicationProfileIds The list of IDs of the application profiles.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the application profiles cannot be removed.
     */
    @DELETE
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeApplicationProfiles(@QueryParam("applicationProfileIds") final List<Long> applicationProfileIds) throws WebAppException {

        RestValidator.validate(new ApplicationProfileIdListValidator(applicationProfileIds));
        applicationProfileManager.removeApplicationProfiles(user.getId(), applicationProfileIds);
        return Response.noContent().build();
    }

    /**
     * Returns the specified application profile.
     * 
     * @param applicationProfileId The long ID of the application profile.
     * @return A response (200) with the specified application profile.
     * @throws WebAppException If the application profile cannot be retrieved.
     */
    @GET
    @Path("/{applicationProfileId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getApplicationProfile(@PathParam("applicationProfileId") final Long applicationProfileId) throws WebAppException {

        return Response.ok(applicationProfileManager.getApplicationProfile(user.getId(), applicationProfileId)).build();
    }

    /**
     * Removes the specified application profile from the current user.
     * 
     * @param applicationProfileId The long ID of the application profile.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the application profile cannot be removed.
     */
    @DELETE
    @Path("/{applicationProfileId}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeApplicationProfile(@PathParam("applicationProfileId") final Long applicationProfileId) throws WebAppException {

        applicationProfileManager.removeApplicationProfile(user.getId(), applicationProfileId);
        return Response.noContent().build();
    }

    /**
     * Updates the specified JAR application profile(s).
     * 
     * @param applicationProfileId The long ID of the application profile.
     * @param applicationFileName The string file name to set.
     * @param applicationProfileName The string application profile name to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the JAR application profile(s) cannot be updated.
     */
    @PUT
    @Path("/jarapplicationprofiles/{applicationProfileId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateJarApplicationProfiles(@PathParam("applicationProfileId") final Long applicationProfileId, @FormParam("applicationFileName") final String applicationFileName,
            @FormParam("applicationProfileName") final String applicationProfileName) throws WebAppException {

        RestValidator.validateOptional(
                new ApplicationProfileFileNameValidator(applicationFileName),
                new ApplicationProfileNameValidator(applicationProfileName));

        applicationProfileManager.updateJarApplicationProfiles(user.getId(), applicationProfileId, applicationFileName, applicationProfileName);
        return Response.noContent().build();
    }

    /**
     * Adds a new native application profile to the current user.
     * 
     * @param applicationProfileName The string application profile name to set.
     * @param commandLine The string command line to set.
     * @param hostAddress The string host address to set.
     * @param portNumber The integer port number to set.
     * @param deviceType The string device type to set.
     * @return A response (201) with the new native application profile that was added.
     * @throws URISyntaxException If a new native application profile cannot be added.
     * @throws WebAppException If a new native application profile cannot be added.
     */
    @POST
    @Path("/nativeapplicationprofiles")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addNativeApplicationProfile(@FormParam("applicationProfileName") final String applicationProfileName, @FormParam("commandLine") final String commandLine,
            @FormParam("hostAddress") final String hostAddress, @FormParam("portNumber") final Integer portNumber, @FormParam("deviceType") final String deviceType)
            throws URISyntaxException, WebAppException {

        RestValidator.validate(
                new ApplicationProfileNameValidator(applicationProfileName),
                new CommandLineValidator(commandLine),
                new HostAddressValidator(hostAddress),
                new PortNumberValidator(portNumber),
                new DeviceTypeValidator(deviceType));

        NativeApplicationProfile nativeApplicationProfile = applicationProfileManager.addNativeApplicationProfile(
                user.getId(), applicationProfileName, commandLine, hostAddress, portNumber, deviceType);
        URI uri = new URI(String.format("/api/applicationprofiles/%d", nativeApplicationProfile.getId()));
        return Response.created(uri).entity(nativeApplicationProfile).build();
    }

    /**
     * Updates the specified native application profile.
     * 
     * @param applicationProfileId The long ID of the application profile.
     * @param applicationProfileName The string application profile name to set.
     * @param commandLine The string command line to set.
     * @param hostAddress The string host address to set.
     * @param portNumber The integer port number to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the native application profile cannot be updated.
     */
    @PUT
    @Path("/nativeapplicationprofiles/{applicationProfileId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateNativeApplicationProfile(@PathParam("applicationProfileId") final Long applicationProfileId, @FormParam("applicationProfileName") final String applicationProfileName,
            @FormParam("commandLine") final String commandLine, @FormParam("hostAddress") final String hostAddress, @FormParam("portNumber") final Integer portNumber) throws WebAppException {

        RestValidator.validateOptional(
                new ApplicationProfileNameValidator(applicationProfileName),
                new CommandLineValidator(commandLine),
                new HostAddressValidator(hostAddress),
                new PortNumberValidator(portNumber));

        applicationProfileManager.updateNativeApplicationProfile(user.getId(), applicationProfileId, applicationProfileName, commandLine, hostAddress, portNumber);
        return Response.noContent().build();
    }

    /**
     * Adds a new remote application profile to the current user.
     * 
     * @param applicationProfileName The string application profile name to set.
     * @param deviceType The string device type to set.
     * @return A response (201) with the new remote application profile that was added.
     * @throws URISyntaxException If a new remote application profile cannot be added.
     * @throws WebAppException If a new remote application profile cannot be added.
     */
    @POST
    @Path("/remoteapplicationprofiles")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addRemoteApplicationProfile(@FormParam("applicationProfileName") final String applicationProfileName, @FormParam("deviceType") final String deviceType)
            throws URISyntaxException, WebAppException {

        RestValidator.validate(
                new ApplicationProfileNameValidator(applicationProfileName),
                new DeviceTypeValidator(deviceType));

        RemoteApplicationProfile remoteApplicationProfile = applicationProfileManager.addRemoteApplicationProfile(user.getId(), applicationProfileName, deviceType);
        URI uri = new URI(String.format("/api/applicationprofiles/%d", remoteApplicationProfile.getId()));
        return Response.created(uri).entity(remoteApplicationProfile).build();
    }

    /**
     * Updates the specified remote application profile.
     * 
     * @param applicationProfileId The long ID of the application profile.
     * @param applicationProfileName The string application profile name to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the remote application profile cannot be updated.
     */
    @PUT
    @Path("/remoteapplicationprofiles/{applicationProfileId}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateRemoteApplicationProfile(@PathParam("applicationProfileId") final Long applicationProfileId, @FormParam("applicationProfileName") final String applicationProfileName)
            throws WebAppException {

        RestValidator.validateOptional(new ApplicationProfileNameValidator(applicationProfileName));
        applicationProfileManager.updateRemoteApplicationProfile(user.getId(), applicationProfileId, applicationProfileName);
        return Response.noContent().build();
    }
}