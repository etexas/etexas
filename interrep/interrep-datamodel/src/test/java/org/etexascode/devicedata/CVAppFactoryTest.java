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
package org.etexascode.devicedata;

import static org.junit.Assert.assertTrue;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.junit.Test;

/**
 * @author janway
 */
public class CVAppFactoryTest {

    @Test
    public void testLoadJars() {
        try {
            InputStream is1 = getClass().getResourceAsStream("/interrep-datamodel-tests.jar");
            byte[] jar1 = IOUtils.toByteArray(is1);

            List<byte[]> jars = new LinkedList<byte[]>();
            jars.add(jar1);

            Map<String, Class<?>> classes = CVAppFactory.loadJars(jars, this.getClass().getClassLoader());

            assertTrue(classes.containsKey("testAppOne"));
            assertTrue(classes.containsKey("TestConnectedVehicleAppImplNoId"));
        }
        catch (FileNotFoundException e2) {
            e2.printStackTrace();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }
}
