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
package org.etexascode.webapp.rest.model;

import javax.xml.bind.annotation.XmlTransient;

/**
 * The model for an on board device.
 * 
 * @author ttevendale
 */
public class OnBoardDevice extends Device {

    /** Serial ID */
    @XmlTransient
    private static final long serialVersionUID = -5821827476862219254L;

    public enum OnBoardDeviceType implements DeviceType {
        OBU,
        CELL
    };

    /**
     * Constructor for an on board device.
     * 
     * @param deviceRuleId The device rule ID
     * @param deviceMac The mac address of this device
     * @param deviceType The type that this device is (OBU, etc...)
     */
    public OnBoardDevice(long deviceRuleId, long deviceMac, OnBoardDeviceType deviceType) {
        super(deviceRuleId, deviceMac, deviceType);
    }
}
