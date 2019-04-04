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
package org.etexascode.webapp.datamodel;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

import org.etexascode.webapp.datamodel.application.ApplicationProfile;

/**
 * A web application user.
 * 
 * @author bbadillo
 * @author bmauldon
 * @author emyers
 */
@Entity
@Table(name = "users")
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class User extends AbstractEntity {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The first name of the user. */
    @Column(name = "first_name")
    private String firstName;

    /** The last name of the user. */
    @Column(name = "last_name")
    private String lastName;

    /** The email address of the user. */
    private String email;

    /** The organization for the user. */
    private String organization;

    /** The username for the user. */
    @Column(unique = true, nullable = false)
    private String username;

    /** The password for the user. */
    private String password;

    /** The authentication token for the user. */
    private String token;

    /** The security role for the user. */
    @Enumerated(EnumType.STRING)
    private UserRole role;

    /** The application profiles for this user. */
    @XmlTransient
    @JoinColumn(name = "user")
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<ApplicationProfile> applicationProfiles = new ArrayList<ApplicationProfile>();

    /** The composites for this user. */
    @XmlTransient
    @JoinColumn(name = "user")
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<Composite> composites = new ArrayList<Composite>();

    /**
     * Returns the first name of the user.
     * 
     * @return The string first name of the user.
     */
    public String getFirstName() {

        return firstName;
    }

    /**
     * Sets the first name of the user.
     * 
     * @param firstName The string first name to set.
     */
    public void setFirstName(String firstName) {

        this.firstName = firstName;
    }

    /**
     * Returns the last name of the user.
     * 
     * @return The string last name of the user.
     */
    public String getLastName() {

        return lastName;
    }

    /**
     * Sets the last name of the user.
     * 
     * @param lastName The string last name to set.
     */
    public void setLastName(String lastName) {

        this.lastName = lastName;
    }

    /**
     * Returns the email address of the user.
     * 
     * @return The string email address of the user.
     */
    public String getEmail() {

        return email;
    }

    /**
     * Sets the email address of the user.
     * 
     * @param email The string email address to set.
     */
    public void setEmail(String email) {

        this.email = email;
    }

    /**
     * Returns the organization for the user.
     * 
     * @return The string organization for the user.
     */
    public String getOrganization() {

        return organization;
    }

    /**
     * Sets the organization for the user.
     * 
     * @param organization The string organization to set.
     */
    public void setOrganization(String organization) {

        this.organization = organization;
    }

    /**
     * Returns the username for the user.
     * 
     * @return The string username for the user.
     */
    public String getUsername() {

        return username;
    }

    /**
     * Sets the username for the user.
     * 
     * @param username The string username to set.
     */
    public void setUsername(String username) {

        this.username = username;
    }

    /**
     * Returns the password for the user.
     * 
     * @return The string password for the user.
     */
    public String getPassword() {

        return password;
    }

    /**
     * Sets the password for the user.
     * 
     * @param password The string password to set.
     */
    public void setPassword(String password) {

        this.password = password;
    }

    /**
     * Returns the authentication token for the user.
     * 
     * @return The string authentication token for the user.
     */
    public String getToken() {

        return token;
    }

    /**
     * Sets the authentication token for the user.
     * 
     * @param token The string authentication token to set.
     */
    public void setToken(String token) {

        this.token = token;
    }

    /**
     * Returns the security role for the user.
     * 
     * @return The security role for the user.
     */
    public UserRole getRole() {

        return role;
    }

    /**
     * Sets the security role for the user.
     * 
     * @param role The security role to set.
     */
    public void setRole(UserRole role) {

        this.role = role;
    }

    /**
     * Returns the application profiles for this user.
     * 
     * @return The list of application profiles for this user.
     */
    public List<ApplicationProfile> getApplicationProfiles() {

        return applicationProfiles;
    }

    /**
     * Sets the application profiles for this user.
     * 
     * @param applicationProfiles The list of application profiles to set.
     */
    public void setApplicationProfiles(List<ApplicationProfile> applicationProfiles) {

        this.applicationProfiles = applicationProfiles;
    }

    /**
     * Returns the composites for this user.
     * 
     * @return The list of composites for this user.
     */
    public List<Composite> getComposites() {

        return composites;
    }

    /**
     * Sets the list of composites for this user.
     * 
     * @param composites The list of composites to set.
     */
    public void setComposites(List<Composite> composites) {

        this.composites = composites;
    }
}