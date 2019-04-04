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
package org.etexascode.appslayerdata;

import java.util.Collection;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;

/**
 * Container for Cellular Device Information at a specific time step
 *
 * @author ttevendale
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class CellularDeviceInfo extends AbstractDeviceInfo {

    /**
     * Serial id.
     */
    private static final long serialVersionUID = -1792850102383807618L;

    /**
     * Info for the cellular device.
     */
    @XmlAnyElement(lax = true)
    final IDistanceable location;

    /**
     * No-arg constructor needed for JAXB because a different constructor is provided too.
     */
    public CellularDeviceInfo() {
        this.location = null;
    }

    /**
     * Constructor
     *
     * @param l The location of this device
     * @param m The messages this device is receiving
     * @param dId The mac address of this device
     */
    public CellularDeviceInfo(IDistanceable l, Collection<BasicMessage> m, long dId) {
        super(m, dId);
        this.location = l;
    }

    /**
     * Determines equality, mainly for testing purposes
     */
    @Override
    public boolean equalsId(IDable entity) {
        return getProperId().equals(entity.getProperId());
    }

    /**
     * Gets the proper id for the cellular device.
     * 
     * @return The proper id.
     */
    @Override
    public String getProperId() {
        return String.format("CellDevice:%d", this.deviceId);
    }

    /**
     * Gets the device location.
     * 
     * @return The location of the device.
     */
    @Override
    public IDistanceable getLocation() {
        return location;
    }
}
