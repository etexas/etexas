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

import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.interceptor.Interceptors;
import javax.json.Json;
import javax.json.JsonObject;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.etexascode.webapp.cdi.CurrentUser;
import org.etexascode.webapp.ejb.UserManager;
import org.etexascode.webapp.exception.WebAppException;
import org.etexascode.webapp.rest.validation.CurrentPasswordValidator;
import org.etexascode.webapp.rest.validation.EmailValidator;
import org.etexascode.webapp.rest.validation.FirstNameValidator;
import org.etexascode.webapp.rest.validation.LastNameValidator;
import org.etexascode.webapp.rest.validation.OrganizationValidator;
import org.etexascode.webapp.rest.validation.PasswordValidator;
import org.etexascode.webapp.rest.validation.RestValidator;
import org.etexascode.webapp.rest.validation.UsernameValidator;

/**
 * The REST service for current user operation requests.
 * 
 * @author emyers
 */
@Path("/api/currentuser")
@RequestScoped
@Interceptors({ StringTrimmer.class })
public class CurrentUserService {

    /** The current user. */
    @Inject
    private CurrentUser user;

    /** The user transaction manager. */
    @Inject
    private UserManager userManager;

    /**
     * Returns the information for the current user.
     * 
     * @return A response (200) with the information for the current user.
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public Response getUser() {

        JsonObject response = Json.createObjectBuilder()
                .add("username", user.getUsername())
                .add("firstName", user.getFirstName())
                .add("lastName", user.getLastName())
                .add("organization", user.getOrganization())
                .add("email", user.getEmail())
                .build();

        return Response.ok(response.toString()).build();
    }

    /**
     * Deletes the account for the current user.
     * 
     * @return A response (204) with no additional content.
     * @throws WebAppException If the user cannot be removed.
     */
    @DELETE
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeUser() throws WebAppException {

        userManager.removeUser(user.getId());
        return Response.noContent().build();
    }

    /**
     * Updates the information for the current user.
     * 
     * @param firstName The string first name to set.
     * @param lastName The string last name to set.
     * @param email The string email address to set.
     * @param organization The string organization to set.
     * @param username The string username to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the information cannot be updated.
     */
    @PUT
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateUser(@FormParam("firstName") final String firstName, @FormParam("lastName") final String lastName, @FormParam("email") final String email,
            @FormParam("organization") final String organization, @FormParam("username") final String username) throws WebAppException {

        RestValidator.validateOptional(
                new FirstNameValidator(firstName),
                new LastNameValidator(lastName),
                new EmailValidator(email),
                new OrganizationValidator(organization),
                new UsernameValidator(username));

        userManager.updateUser(user.getId(), firstName, lastName, email, organization, username);
        return Response.noContent().build();
    }

    /**
     * Updates the password for the current user.
     * 
     * @param currentPassword The string value of the current password.
     * @param newPassword The string password to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the password cannot be updated.
     */
    @PUT
    @Path("/password")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response updateUser(@FormParam("currentPassword") final String currentPassword, @FormParam("newPassword") final String newPassword) throws WebAppException {

        RestValidator.validate(
                new CurrentPasswordValidator(currentPassword),
                new PasswordValidator(newPassword));

        userManager.updateUser(user.getId(), currentPassword, newPassword);
        return Response.noContent().build();
    }

    /**
     * Logs the current user out of the application.
     * 
     * @return A response (204) with no additional content.
     * @throws WebAppException If the user cannot be logged out.
     */
    @POST
    @Path("/logout")
    @Produces(MediaType.APPLICATION_JSON)
    public Response logout() throws WebAppException {

        userManager.logout(user.getUsername());
        return Response.noContent().build();
    }
}
