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

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Test for Config bean for project configuration file.
 * 
 * @author bmauldon
 */
public class ConfigTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(ConfigTest.class);

    public static <T> void writeJaxb(T manager, String xmlfile, Class<T> clazz) {
        try {
            /** Set up Jaxb */
            JAXBContext jc = JAXBContext.newInstance(clazz);
            Marshaller marshaller = jc.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
            /** Set up file IO */
            OutputStream os;
            File file = new File(xmlfile);
            if (file.exists()) {
                file.delete();
            }
            os = new FileOutputStream(file);
            /** Write out class as XML file */
            marshaller.marshal(manager, os);
        }
        catch (FileNotFoundException e) {
            LOGGER.debug("Could not create xmlfile");
        }
        catch (JAXBException ex) {
            LOGGER.debug(ex.toString());

        }
    }

    List<Integer> iseed = new ArrayList<Integer>();

    long seed = randomLong();

    Random random = new Random(seed);

    int n = 21;

    Integer N = new Integer(n);

    Config config = new Config();

    /**
     * Based on George Marsaglia's XORShift Random Number Generators Produces medium quality random
     * Numbers- period 2exp(64)-1
     * 
     * @return Random long.
     */
    public long randomLong() {
        long x = System.nanoTime();
        x ^= x << 21;
        x ^= x >>> 35;
        x ^= x << 4;
        return x;
    }

    @Before
    public void setUp() throws Exception {
        for (int i = 0; i < n; i++) {
            iseed.add(new Integer(random.nextInt()));
            // LOGGER.debug(iseed.get(i).toString());
        }

    }

    @After
    public void tearDown() throws Exception {}

    @Test
    public void testGetISEED() {
        config.setISEED(iseed);

        assertEquals(iseed, config.getISEED());
    }

    @Test
    public void testGetNUMREP() {
        config.setNUMREP(N);
        assertEquals(N, config.getNUMREP());
    }

    @Test
    public void testJaxb() {
        Integer numRep = n - 1;
        config.setNUMREP(numRep);
        config.setISEED(iseed);
        writeJaxb(config, "config.xml", Config.class);

    }

    @Test
    public void testSetISEED() {
        config.setISEED(iseed);

        assertEquals(iseed, config.getISEED());
    }

    @Test
    public void testSetNUMREP() {
        config.setNUMREP(N);
        assertEquals(N, config.getNUMREP());
    }
}
