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

package org.etexascode.webapp.ra.api.config;

import java.io.Serializable;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

/**
 * An object used to hold data back from the configuration file for the simulation with multiple
 * executions.
 * 
 * @author amauldon
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
public class Config implements Serializable {

    /** Serial ID */
    @XmlTransient
    private static final long serialVersionUID = 1L;

    /** Number of executions run */
    @XmlElement
    private Integer NUMREP;

    /** List of Random seeds */
    @XmlElement
    private List<Integer> ISEED;

    public List<Integer> getISEED() {
        return ISEED;
    }

    public Integer getNUMREP() {
        return NUMREP;
    }

    public void setISEED(List<Integer> iSEED) {
        ISEED = iSEED;
    }

    public void setNUMREP(Integer nUMREP) {
        NUMREP = nUMREP;
    }
}
