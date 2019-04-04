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
package org.etexascode.test.restharness.templateclasses;

/**
 * Info for how to connect to the server.
 * 
 * @author ablatt
 */
public class ClientInfo {

    /**
     * The user name to use.
     */
    String uname;

    /**
     * The password to use.
     */
    String pass;

    /**
     * The location of the server.
     */
    String hostLocation;

    /**
     * Generic Constructor.
     */
    public ClientInfo() {
        uname = null;
        pass = null;
        hostLocation = null;
    }

    /**
     * Constructor with values.
     * 
     * @param uname The user name to use.
     * @param pass The password to use.
     * @param hostLocation The location of the server.
     */
    public ClientInfo(String uname, String pass, String hostLocation) {
        this.uname = uname;
        this.pass = pass;
        this.hostLocation = hostLocation;
    }

    /**
     * Getter.
     * 
     * @return The user name to use.
     */
    public String getUname() {
        return uname;
    }

    /**
     * Setter.
     * 
     * @param uname The user name to use.
     */
    public void setUname(String uname) {
        this.uname = uname;
    }

    /**
     * Getter.
     * 
     * @return The password to use.
     */
    public String getPass() {
        return pass;
    }

    /**
     * Setter.
     * 
     * @param pass The password to use.
     */
    public void setPass(String pass) {
        this.pass = pass;
    }

    /**
     * Getter.
     * 
     * @return The location of the server.
     */
    public String getHostLocation() {
        return hostLocation;
    }

    /**
     * Setter.
     * 
     * @param hostLocation The location of the server.
     */
    public void setHostLocation(String hostLocation) {
        this.hostLocation = hostLocation;
    }
}
