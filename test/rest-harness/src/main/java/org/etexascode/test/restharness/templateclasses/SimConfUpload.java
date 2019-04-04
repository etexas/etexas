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

import java.io.File;

/**
 * Create a sim config from an archive.
 * 
 * @author ablatt
 */
public class SimConfUpload extends SimConfBase {

    /**
     * The archive containing the basis of the sim config.
     */
    File archive;

    /**
     * The type of sim config this archive represents.
     */
    String type;

    /**
     * @param name The name to be used for the sim config.
     * @param archive The archive containing the basis of the sim config.
     * @param archiveType The type of sim config this archive represents.
     */
    public SimConfUpload(String name, File archive, String archiveType) {
        super(name);
        this.archive = archive;
        type = archiveType;
    }

    /**
     * Getter.
     * 
     * @return The archive containing the basis of the sim config.
     */
    public File getArchive() {
        return archive;
    }

    /**
     * Setter.
     * 
     * @param archive The archive containing the basis of the sim config.
     */
    public void setArchive(File archive) {
        this.archive = archive;
    }

    /**
     * Getter.
     * 
     * @return The type of sim config this archive represents.
     */
    public String getType() {
        return type;
    }

    /**
     * Setter.
     * 
     * @param type The type of sim config this archive represents.
     */
    public void setType(String type) {
        this.type = type;
    }
}
