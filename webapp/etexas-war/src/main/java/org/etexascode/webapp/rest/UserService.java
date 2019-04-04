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

import java.util.UUID;

import javax.annotation.Resource;
import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.interceptor.Interceptors;
import javax.json.Json;
import javax.json.JsonObject;
import javax.mail.Session;
import javax.servlet.ServletContext;
import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.etexascode.webapp.datamodel.User;
import org.etexascode.webapp.ejb.UserManager;
import org.etexascode.webapp.exception.WebAppException;
import org.etexascode.webapp.rest.validation.AccountRecoveryTypeValidator;
import org.etexascode.webapp.rest.validation.EmailValidator;
import org.etexascode.webapp.rest.validation.FirstNameValidator;
import org.etexascode.webapp.rest.validation.LastNameValidator;
import org.etexascode.webapp.rest.validation.OrganizationValidator;
import org.etexascode.webapp.rest.validation.PasswordValidator;
import org.etexascode.webapp.rest.validation.RestValidator;
import org.etexascode.webapp.rest.validation.UsernameValidator;
import org.etexascode.webapp.util.AccountProcessor;
import org.slf4j.LoggerFactory;

/**
 * The REST service for user operation requests.
 * 
 * @author bbadillo
 * @author keagan
 * @author emyers
 */
@Path("/users")
@RequestScoped
@Interceptors({ StringTrimmer.class })
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public class UserService {

    /** The Java mail session name. */
    private static final String JAVA_MAIL_NAME = "java:/Mail";

    /** The Java mail session. */
    @Resource(mappedName = JAVA_MAIL_NAME)
    private Session mappedSession;

    /** The user transaction manager. */
    @Inject
    private UserManager userManager;

    /** The servlet context */
    @Inject
    private ServletContext context;

    /**
     * Registers a new web application user.
     * 
     * @param firstName The string first name of the user.
     * @param lastName The string last name of the user.
     * @param email The string email address of the user.
     * @param organization The string organization for the user.
     * @param username The string username for the user.
     * @param password The string password for the user.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the user cannot be registered.
     */
    @POST
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces({ MediaType.APPLICATION_JSON })
    public Response addUser(@FormParam("firstName") final String firstName, @FormParam("lastName") final String lastName, @FormParam("email") final String email,
            @FormParam("organization") final String organization, @FormParam("username") final String username, @FormParam("password") final String password) throws WebAppException {

        RestValidator.validate(
                new FirstNameValidator(firstName),
                new LastNameValidator(lastName),
                new EmailValidator(email),
                new OrganizationValidator(organization),
                new UsernameValidator(username),
                new PasswordValidator(password));

        String registerToken = UUID.randomUUID().toString();
        String host = context.getInitParameter("VisibleHostName");
        String port = context.getInitParameter("VisibleHostPort");

        userManager.addUser(firstName, lastName, email, organization, username, password, registerToken);

        try {

            AccountProcessor.sendVerificationEmail(mappedSession, "http://" + host + ":" + port, email, registerToken);
        }
        catch (RuntimeException exception) {

            userManager.removeUser(username);
            LoggerFactory.getLogger(UserService.class).error("Registration Failed", exception);
            throw new WebAppException("Registration Failed", "An unexpected error has occurred.");
        }

        return Response.noContent().build();
    }

    /**
     * Authenticates the specified username and password combination. If successful, an
     * authentication token will be returned that can be used in subsequent requests that require
     * user authentication (i.e., API requests).
     * 
     * @param username The string username to authenticate.
     * @param password The string password to authenticate.
     * @return A response (200) with the authenticated username and authentication token.
     * @throws WebAppException If the username and password cannot be authenticated.
     */
    @POST
    @Path("/login")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response login(@FormParam("username") final String username, @FormParam("password") final String password) throws WebAppException {

        RestValidator.validate(
                new UsernameValidator(username),
                new PasswordValidator(password));

        JsonObject response = Json.createObjectBuilder()
                .add("username", username)
                .add("token", userManager.login(username, password))
                .build();

        return Response.ok(response.toString()).build();
    }

    /**
     * Recovers the username/password for the user with the specified email address.
     * 
     * @param email The string email address for the user.
     * @param recoveryType The recovery type (username or password).
     * @return A response (204) with no additional content.
     * @throws WebAppException If the username/password cannot be recovered.
     */
    @PUT
    @Path("/recovery")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response recovery(@FormParam("email") final String email, @FormParam("recoveryType") final String recoveryType) throws WebAppException {

        RestValidator.validate(
                new EmailValidator(email),
                new AccountRecoveryTypeValidator(recoveryType));

        User user;
        String exceptionTitle = "username".equals(recoveryType) ? "Recover Username Failure" : "Recover Password Failure";

        try {

            user = userManager.getUserFromEmail(email);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        if ("username".equals(recoveryType)) {

            try {

                AccountProcessor.sendUsernameRecoveryEmail(mappedSession, email, user.getUsername());
            }
            catch (RuntimeException exception) {

                LoggerFactory.getLogger(UserService.class).error(exceptionTitle, exception);
                throw new WebAppException(exceptionTitle, String.format("The username for \"%s\" could not be recovered.", email));
            }
        }
        else {

            String host = context.getInitParameter("VisibleHostName");
            String port = context.getInitParameter("VisibleHostPort");
            String password = userManager.resetPassword(user.getId());

            try {

                AccountProcessor.sendPasswordRecoveryEmail(mappedSession, String.format("http://%s:%s", host, port), email, password);
            }
            catch (RuntimeException exception) {

                LoggerFactory.getLogger(UserService.class).error(exceptionTitle, exception);
                throw new WebAppException(exceptionTitle, String.format("The password for \"%s\" could not be recovered.", email));
            }
        }

        return Response.noContent().build();

    }

    /**
     * Verifies the email address of a new web application user.
     * 
     * @param email The string email address for the user.
     * @param token The string registration token.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the email address cannot be verified.
     */
    @PUT
    @Path("/verify")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.APPLICATION_JSON)
    public Response verifyEmail(@FormParam("email") final String email, @FormParam("token") final String token) throws WebAppException {

        RestValidator.validate(new EmailValidator(email));
        userManager.verifyEmail(email, token);
        return Response.noContent().build();
    }
}