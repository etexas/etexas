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

package org.etexascode.nonstd;

import java.io.Serializable;
import java.util.Date;
import java.util.HashMap;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * DetectorData contains a map that holds all SingleDetectorData objects
 * 
 * @author dranker
 * @author bbadillo
 */
@XmlRootElement
public class DetectorData extends Message implements Serializable {

    private HashMap<Integer, SingleDetectorData> detectors;

    private long messageTime = new Date().getTime();

    /**
     * Default noarg constructor for serialization
     */
    public DetectorData() {}

    /**
     * Takes size of detector data and adds to hashmap
     * 
     * @param size Size of data
     */
    public DetectorData(int size) {
        detectors = new HashMap<Integer, SingleDetectorData>();
        for (int i = 0; i < size; i++) {
            detectors.put(i, new SingleDetectorData());
        }
    }

    /**
     * Gets the detectors from hashmap and returns them
     * 
     * @return detectors in the hashmap
     */
    public HashMap<Integer, SingleDetectorData> getDetectors() {
        return detectors;
    }

    /**
     * Set the detectors in the hashmap
     * 
     * @param detectors the detectors to add to the hashmap
     */
    public void setDetectors(HashMap<Integer, SingleDetectorData> detectors) {
        this.detectors = detectors;
    }

    /**
     * Gets the message time
     * 
     * @return the message time
     */
    public long getMessageTime() {
        return messageTime;
    }

}
