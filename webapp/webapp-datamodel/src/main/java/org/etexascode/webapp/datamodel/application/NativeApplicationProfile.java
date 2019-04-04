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
package org.etexascode.webapp.datamodel.application;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * A native connected vehicle application profile.
 * 
 * @author janway
 * @author emyers
 */
@Entity
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@DiscriminatorValue(ApplicationType.Discriminator.NATIVE)
public class NativeApplicationProfile extends ApplicationProfile {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The command line for this native application profile. */
    @Column(name = "command_line")
    private String commandLine;

    /** The host address for this native application profile. */
    @Column(name = "host_address")
    private String hostAddress;

    /** The port number for this native application profile. */
    @Column(name = "port_number")
    private int portNumber;

    /**
     * Returns the command line for this native application profile.
     * 
     * @return The string command line for this native application profile.
     */
    public String getCommandLine() {

        return commandLine;
    }

    /**
     * Sets the command line for this native application profile.
     * 
     * @param commandLine The string command line to set.
     */
    public void setCommandLine(String commandLine) {

        this.commandLine = commandLine;
    }

    /**
     * Returns the host address for this native application profile.
     * 
     * @return The string host address for this native application profile.
     */
    public String getHostAddress() {

        return hostAddress;
    }

    /**
     * Sets the host address for this native application profile.
     * 
     * @param hostAddress The string host address to set.
     */
    public void setHostAddress(String hostAddress) {

        this.hostAddress = hostAddress;
    }

    /**
     * Returns the port number for this native application profile.
     * 
     * @return The integer port number for this native application profile.
     */
    public int getPortNumber() {

        return portNumber;
    }

    /**
     * Sets the port number for this native application profile.
     * 
     * @param portNumber The integer port number to set.
     */
    public void setPortNumber(int portNumber) {

        this.portNumber = portNumber;
    }

    @Override
    protected NativeApplication createApplicationInternal() {

        NativeApplication application = new NativeApplication();
        application.setCommandLine(commandLine);
        application.setHostAddress(hostAddress);
        application.setPortNumber(portNumber);

        return application;
    }
}