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

package org.etexascode.texas.gdvsim;

import java.io.FileNotFoundException;
import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Overrides GDVSIM so it can be setup from the web.
 * 
 * @author bbadillo
 */
public class OverriddenGDVSIM extends gdvsim {

    private final Webstarter webstarter;

    /** Static logger */
    private static final Logger LOGGER = LoggerFactory.getLogger(OverriddenGDVSIM.class);

    /**
     * Important! This is here to initialize the intersection in the gdvsim class before the
     * overridden code runs.
     */
    static {
        Intersection.init(null);

        if (Intersection.msiv_returnCode == Intersection.RETURN_SUCCESS) {
            Intersection.mbov_debug_calculateGraphics = false;
            Intersection.mbov_debug_checkForErrorsGDV = false;
            Intersection.mbov_debug_checkForErrorsSIM = false;
            Intersection.mbov_debug_filesReadGDV = false;
            Intersection.mbov_debug_filesReadSIM = false;
            Intersection.mbov_debug_filesReadSketch = false;
            Intersection.mbov_debug_filesWriteDXF = false;
            Intersection.mbov_debug_filesWriteGDV = false;
            Intersection.mbov_debug_filesWriteSIM = false;
            Intersection.mbov_debug_GDVCON = false;
            Intersection.mbov_debug_init = false;
            Intersection.mbov_debug_menu = false;
            Intersection.mbov_debug_other = false;
            Intersection.mbov_debug_paint = false;
            Intersection.mbov_debug_printGDV = false;
            Intersection.mbov_debug_printSIM = false;
            Intersection.mbov_debug_readFromCard = false;
            Intersection.mbov_debug_readFromCard1 = false;
            Intersection.mbov_debug_setAllInvalid = false;
            Intersection.mbov_debug_writeToCard = false;
            Intersection.mbov_debug_writeToCard1 = false;
        }
    }

    /**
     * @param webstarter The web starter.
     * @param argsOne The first arguments.
     * @param argsTwo The second arguments.
     */

    public OverriddenGDVSIM(Webstarter webstarter, String argsOne, String argsTwo) {
        super(argsOne, argsTwo);
        this.webstarter = webstarter;

        activateMenuItem.setEnabled(false);
        newMenuItem.setEnabled(false);
        openMenu.setEnabled(false);
        libraryMenuItem.setEnabled(false);
        existingFileMenuItem.setEnabled(false);
        dataMenuItem.setEnabled(false);
        printoutMenuItem.setEnabled(false);
        existingFileMenuItem.setEnabled(false);
        DXFMenuItem.setEnabled(false);
        imageMenu.setEnabled(false);
        attachImageFileMenuItem.setEnabled(false);
    }

    @Override
    void SaveAction() {
        super.SaveAction();
        try {
            webstarter.sendProjectFiles();
        }
        catch (FileNotFoundException ex) {
            LOGGER.debug(ex.toString());
        }
        catch (IOException ex) {
            LOGGER.debug(ex.toString());

        }
    }

}
