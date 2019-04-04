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

package org.etexascode.webapp.cdi;

import java.io.Serializable;

import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;

import org.etexascode.webapp.datamodel.User;
import org.etexascode.webapp.datamodel.UserRole;
import org.etexascode.webapp.ejb.UserManager;
import org.etexascode.webapp.exception.WebAppException;

/**
 * The wrapper class to inject the current user.
 * 
 * @author bbadillo
 * @author dranker
 * @author emyers
 */
@RequestScoped
public class CurrentUser implements Serializable {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The current user. */
    private User user;

    /** The user transaction manager. */
    @Inject
    private UserManager userManager;

    /**
     * Initializes the current user.
     * 
     * @param username The string username of the user to initialize.
     */
    public void initUser(String username) {

        try {

            user = userManager.getUserFromUsername(username);
        }
        catch (WebAppException exception) {

            // the user will remain null so no action is necessary
        }
    }

    /**
     * Returns the current user.
     * 
     * @return The current user.
     */
    public User getUser() {

        return user;
    }

    /**
     * Returns the ID of the current user.
     * 
     * @return The long ID of the current user.
     */
    public Long getId() {

        return user.getId();
    }

    /**
     * Returns the first name of the current user.
     * 
     * @return The string first name of the current user.
     */
    public String getFirstName() {

        return user.getFirstName();
    }

    /**
     * Returns the last name of the current user.
     * 
     * @return The string last name of the current user.
     */
    public String getLastName() {

        return user.getLastName();
    }

    /**
     * Returns the email address of the current user.
     * 
     * @return The string email address of the current user.
     */
    public String getEmail() {

        return user.getEmail();
    }

    /**
     * Returns the organization for the current user.
     * 
     * @return The string organization for the current user.
     */
    public String getOrganization() {

        return user.getOrganization();
    }

    /**
     * Returns the username for the current user.
     * 
     * @return The string username for the current user.
     */
    public String getUsername() {

        return user.getUsername();
    }

    /**
     * Returns the password for the current user.
     * 
     * @return The string password for the current user.
     */
    public String getPassword() {

        return user.getPassword();
    }

    /**
     * Returns the authentication token for the current user.
     * 
     * @return The string authentication token for the current user.
     */
    public String getToken() {

        return user.getToken();
    }

    /**
     * Returns the security role for the current user.
     * 
     * @return The security role for the current user.
     */
    public UserRole getRole() {

        return user.getRole();
    }
}
