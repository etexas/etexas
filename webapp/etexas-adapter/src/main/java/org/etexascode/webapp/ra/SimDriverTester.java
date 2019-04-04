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

package org.etexascode.webapp.ra;

import java.io.File;
import java.io.IOException;

/**
 * A driver for running and controlling eTEXAS using standard I/O. This entire class should be
 * package-private.
 * 
 * @author bbadillo
 */
public class SimDriverTester {

    /**
     * The main class which starts a simulator.
     * 
     * @param args The command line arguments.
     * @throws IOException If an IO exception occurs.
     * @throws InterruptedException If an interrupted exception occurs.
     */
    public static void main(String[] args) throws IOException, InterruptedException {
        SimProcessRunner runner1 = null;
        SimProcessRunner runner2 = null;
        try {
            runner1 = new SimProcessRunner("etexas-test-id1", 1099, "C:\\texas\\exe", new File("C:\\texas\\projects\\demo"), "C:\\texas\\sys_dat", null, null);
            runner2 = new SimProcessRunner("etexas-test-id2", 1099, "C:\\texas\\exe", new File("C:\\texas\\projects\\demo"), "C:\\texas\\sys_dat", null, null);

            runner1.start();

            Thread.sleep(1000);

            // runner1.connectToSimDriver().nextStep(1);

            runner2.start();

            Thread.sleep(1000);

            // runner2.connectToSimDriver().nextStep(1);

            Thread.sleep(1000);

            // runner1.connectToSimDriver().nextStep(1);

            Thread.sleep(1000);

            // runner2.connectToSimDriver().nextStep(1);

            Thread.sleep(3000);
        }
        finally {
            try {
                if (runner1 != null) {
                    runner1.stop();
                }
            }
            finally {
                if (runner2 != null) {
                    runner2.stop();
                }
            }
        }
    }
}
