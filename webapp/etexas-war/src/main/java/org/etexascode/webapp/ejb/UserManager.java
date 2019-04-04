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
package org.etexascode.webapp.ejb;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.ws.rs.core.Response;

import org.etexascode.webapp.datamodel.Composite;
import org.etexascode.webapp.datamodel.Execution;
import org.etexascode.webapp.datamodel.User;
import org.etexascode.webapp.datamodel.UserRole;
import org.etexascode.webapp.exception.WebAppException;
import org.etexascode.webapp.util.PasswordGenerator;
import org.etexascode.webapp.util.StringEncoder;

/**
 * The EJB to manage user database transactions.
 * 
 * @author bbadillo
 * @author janway
 * @author bmauldon
 * @author emyers
 */
@Stateless
public class UserManager {

    /** The database entity manager. */
    @PersistenceContext(unitName = "etexas-pu")
    private EntityManager entityManager;

    /**
     * Returns the specified user.
     * 
     * @param userId The long ID of the user.
     * @return The user with the specified attributes.
     * @throws WebAppException If no such user exists.
     */
    public User getUser(Long userId) throws WebAppException {

        User user = entityManager.find(User.class, userId);

        if (user == null) {

            throw new WebAppException("Get User Failure", String.format("No user with ID \"%d\" could be found.", userId), Response.Status.NOT_FOUND);
        }

        return user;
    }

    /**
     * Returns the specified user.
     * 
     * @param email The string email address for the user.
     * @return The user with the specified attributes.
     * @throws WebAppException If no such user exists.
     */
    public User getUserFromEmail(String email) throws WebAppException {

        List<User> users = entityManager.createNamedQuery(Query.USERS_FROM_EMAIL, User.class)
                .setParameter(QueryParameter.EMAIL, email)
                .getResultList();

        if (users.isEmpty()) {

            throw new WebAppException("Get User from Email Failure", String.format("No user with email \"%s\" could be found.", email), Response.Status.NOT_FOUND);
        }

        return users.get(0);
    }

    /**
     * Returns the specified user.
     * 
     * @param username The string username for the user.
     * @return The user with the specified attributes.
     * @throws WebAppException If no such user exists.
     */
    public User getUserFromUsername(String username) throws WebAppException {

        List<User> users = entityManager.createNamedQuery(Query.USERS_FROM_USERNAME, User.class)
                .setParameter(QueryParameter.USERNAME, username)
                .getResultList();

        if (users.isEmpty()) {

            throw new WebAppException("Get User from Username Failure", String.format("No user with username \"%s\" could be found.", username), Response.Status.NOT_FOUND);
        }

        return users.get(0);
    }

    /**
     * Adds a new user to the database.
     * 
     * @param firstName The string first name to set.
     * @param lastName The string last name to set.
     * @param email The string email address to set.
     * @param organization The string organization to set.
     * @param username The string username to set.
     * @param password The string password to set.
     * @param token The string verification token to set.
     * @return The new user that was added.
     * @throws WebAppException If a new user cannot be added.
     */
    public User addUser(String firstName, String lastName, String email, String organization, String username, String password, String token) throws WebAppException {

        try {

            validateUsername(username);
            validateEmail(email);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Add User Failure", exception.getMessage(), exception.getStatus());
        }

        User user = new User();
        user.setFirstName(firstName);
        user.setLastName(lastName);
        user.setEmail(email);
        user.setOrganization(organization);
        user.setUsername(username);
        user.setPassword(StringEncoder.hash(password));
        user.setRole(UserRole.UNVERIFIED);
        user.setToken(token);
        entityManager.persist(user);

        return user;
    }

    /**
     * Removes the specified user from the database.
     * 
     * @param userId The long ID of the user.
     * @throws WebAppException If the user cannot be removed.
     */
    public void removeUser(Long userId) throws WebAppException {

        try {

            removeUser(getUser(userId));
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove User Failure", exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Removes the specified user from the database.
     * 
     * @param username The string username for the user.
     * @throws WebAppException If the user cannot be removed.
     */
    public void removeUser(String username) throws WebAppException {

        try {

            removeUser(getUserFromUsername(username));
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove User Failure", exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Removes the specified users from the database.
     * 
     * @param usernames A string list of usernames to remove.
     * @throws WebAppException If any of the users cannot be removed.
     */
    public void removeUsers(List<String> usernames) throws WebAppException {

        List<User> users = new ArrayList<User>();

        for (String username : usernames) {

            try {

                users.add(getUserFromUsername(username.trim()));
            }
            catch (WebAppException exception) {

                throw new WebAppException("Remove Users Failure", exception.getMessage(), exception.getStatus());
            }
        }

        for (User user : users) {

            removeUser(user);
        }
    }

    /**
     * Removes the specified user from the database.
     * 
     * @param user The user to remove.
     */
    private void removeUser(User user) {

        List<Long> executions = new ArrayList<Long>();

        for (Composite composite : user.getComposites()) {

            for (Execution execution : composite.getExecutions()) {

                executions.add(execution.getId());
            }
        }

        if (!executions.isEmpty()) {

            entityManager.createNamedQuery(Query.DELETE_APPLICATION_LOGS_FROM_EXECUTION_LIST)
                    .setParameter(QueryParameter.EXECUTION_LIST, executions)
                    .executeUpdate();

            entityManager.createNamedQuery(Query.DELETE_COMMANDS_FROM_EXECUTION_LIST)
                    .setParameter(QueryParameter.EXECUTION_LIST, executions)
                    .executeUpdate();
        }

        entityManager.remove(user);
    }

    /**
     * Updates the password for the specified user.
     * 
     * @param userId The long ID of the user.
     * @param currentPassword The string value of the current password.
     * @param newPassword The string password to set.
     * @throws WebAppException If the password cannot be updated.
     */
    public void updateUser(Long userId, String currentPassword, String newPassword) throws WebAppException {

        User user;
        String exceptionTitle = "Update Password Failure";

        try {

            user = getUser(userId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        if (!user.getPassword().equals(StringEncoder.hash(currentPassword))) {

            throw new WebAppException(exceptionTitle, "The provided password does not match the current password.");
        }

        if (user.getPassword().equals(StringEncoder.hash(newPassword))) {

            throw new WebAppException(exceptionTitle, "The new password may not match the current password.");
        }

        user.setPassword(StringEncoder.hash(newPassword));
    }

    /**
     * Updates the information for the specified user.
     * 
     * @param userId The long ID of the user.
     * @param firstName The string first name to set.
     * @param lastName The string last name to set.
     * @param email The string email address to set.
     * @param organization The string organization to set.
     * @param username The string username to set.
     * @throws WebAppException If the information cannot be updated.
     */
    public void updateUser(Long userId, String firstName, String lastName, String email, String organization, String username) throws WebAppException {

        User user;

        try {

            user = getUser(userId);
            firstName = (firstName != null) ? firstName : user.getFirstName();
            lastName = (lastName != null) ? lastName : user.getLastName();
            email = (email != null) ? email : user.getEmail();
            organization = (organization != null) ? organization : user.getOrganization();
            username = (username != null) ? username : user.getUsername();

            if (!user.getUsername().equalsIgnoreCase(username)) {

                validateUsername(username);
            }

            if (!user.getEmail().equalsIgnoreCase(email)) {

                validateEmail(email);
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException("Update Information Failure", exception.getMessage(), exception.getStatus());
        }

        user.setFirstName(firstName);
        user.setLastName(lastName);
        user.setEmail(email);
        user.setOrganization(organization);
        user.setUsername(username);
    }

    /**
     * Authenticates the specified username and password combination.
     * 
     * @param username The string username to authenticate.
     * @param password The string password to authenticate.
     * @return The string authentication token that was generated.
     * @throws WebAppException If the username and password cannot be authenticated.
     */
    public String login(String username, String password) throws WebAppException {

        User user;
        String exceptionTitle = "Login Failure";
        String exceptionMessage = "The provided username and password combination is not valid.";

        try {

            user = getUserFromUsername(username);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exceptionMessage);
        }

        if (!user.getPassword().equals(StringEncoder.hash(password))) {

            user.setToken(null);
            throw new WebAppException(exceptionTitle, exceptionMessage);
        }

        if (user.getRole() == UserRole.UNVERIFIED) {

            throw new WebAppException(exceptionTitle, String.format("The email address for \"%s\" has not been verified.", username));
        }

        String token = UUID.randomUUID().toString();
        user.setToken(StringEncoder.hash(token));

        return token;
    }

    /**
     * Resets the authentication token for the specified user.
     * 
     * @param username The string username for the user.
     * @throws WebAppException If the user cannot be found.
     */
    public void logout(String username) throws WebAppException {

        User user;

        try {

            user = getUserFromUsername(username);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Logout Failure", exception.getMessage(), exception.getStatus());
        }

        user.setToken(null);
    }

    /**
     * Resets the password for the specified user.
     * 
     * @param userId The long ID of the user.
     * @return The new password that was generated.
     * @throws WebAppException If the password cannot be reset.
     */
    public String resetPassword(Long userId) throws WebAppException {

        User user;

        try {

            user = getUser(userId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Reset Password Failure", exception.getMessage(), exception.getStatus());
        }

        String password = PasswordGenerator.generatePassword();
        user.setPassword(StringEncoder.hash(password));

        return password;
    }

    /**
     * Verifies the specified email address and updates the security role of the associated user.
     * The method does not require a verification token and should only be invoked when
     * administrative privileges have been verified.
     * 
     * @param email The string email address to verify.
     * @throws WebAppException If the email address cannot be verified.
     */
    public void verifyEmail(String email) throws WebAppException {

        User user;

        try {

            user = getUserFromEmail(email);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Verify Email Failure", exception.getMessage(), exception.getStatus());
        }

        verifyEmail(email, user.getToken());
    }

    /**
     * Verifies the specified email address and updates the security role of the associated user.
     * 
     * @param email The string email address to verify.
     * @param token The string verification token.
     * @throws WebAppException If the email address cannot be verified.
     */
    public void verifyEmail(String email, String token) throws WebAppException {

        User user;
        String exceptionTitle = "Verify Email Failure";

        try {

            user = getUserFromEmail(email);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        if (user.getRole() != UserRole.UNVERIFIED) {

            throw new WebAppException(exceptionTitle, String.format("The email address \"%s\" has already been verified.", email));
        }
        else if (!user.getToken().equals(token)) {

            throw new WebAppException(exceptionTitle, "The provided email verification token does not match the provided email address.");
        }

        user.setRole(UserRole.ETEXAS_USER);
    }

    /**
     * Validates the specified email address.
     * 
     * @param email The string email address to validate.
     * @throws WebAppException If the email address is not unique.
     */
    public void validateEmail(String email) throws WebAppException {

        List<User> users = entityManager.createNamedQuery(Query.USERS_FROM_EMAIL, User.class)
                .setParameter(QueryParameter.EMAIL, email)
                .getResultList();

        if (!users.isEmpty()) {

            throw new WebAppException("Validate Email Failure", String.format("A user with the email \"%s\" already exists.", email));
        }
    }

    /**
     * Validates the specified username.
     * 
     * @param username The string username to validate.
     * @throws WebAppException If the username is not unique.
     */
    public void validateUsername(String username) throws WebAppException {

        List<User> users = entityManager.createNamedQuery(Query.USERS_FROM_USERNAME, User.class)
                .setParameter(QueryParameter.USERNAME, username)
                .getResultList();

        if (!users.isEmpty()) {

            throw new WebAppException("Validate Username Failure", String.format("A user with the username \"%s\" already exists.", username));
        }
    }
}