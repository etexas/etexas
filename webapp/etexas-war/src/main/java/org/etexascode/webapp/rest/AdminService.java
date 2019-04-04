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

import java.util.List;

import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.interceptor.Interceptors;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.etexascode.webapp.ejb.UserManager;
import org.etexascode.webapp.exception.WebAppException;
import org.etexascode.webapp.rest.validation.EmailValidator;
import org.etexascode.webapp.rest.validation.RestValidator;
import org.etexascode.webapp.rest.validation.UsernameListValidator;

/**
 * The REST service for administrative operation requests.
 * 
 * @author emyers
 */
@Path("/admin")
@RequestScoped
@Interceptors({ StringTrimmer.class })
public class AdminService {

    /** The user transaction manager. */
    @Inject
    private UserManager userManager;

    /**
     * Removes the users with the specified usernames.
     * 
     * @param usernames The list of usernames to remove.
     * @return A response (204) with no additional content.
     * @throws WebAppException If any user cannot be removed.
     */
    @DELETE
    @Path("/users")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeUsers(@QueryParam("usernames") final List<String> usernames) throws WebAppException {

        RestValidator.validate(new UsernameListValidator(usernames));
        userManager.removeUsers(usernames);
        return Response.noContent().build();
    }

    /**
     * Verifies the email address of a new web application user.
     * 
     * @param email The string email address for the user.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the email address cannot be verified.
     */
    @PUT
    @Path("/verify")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response verifyUser(@FormParam("email") final String email) throws WebAppException {

        RestValidator.validate(new EmailValidator(email));
        userManager.verifyEmail(email);
        return Response.noContent().build();
    }
}
