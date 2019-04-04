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
package org.etexascode.webapp.playback;

import static org.junit.Assert.fail;

import java.io.File;
import java.net.URL;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class PlaybackSimulatorTest {

    String inputDirectory = "/playbackTestInput";

    String inputDirectory2 = "/playbackTestInput2";

    String inputDirectoryStatic = inputDirectory + "/static_data";

    String inputDirectoryStep = inputDirectory + "/step_data";

    File input;

    File input2;

    PlaybackSimulator playsim;

    @Before
    public void setUp() {
        URL url = PlaybackSimulatorTest.class.getResource(inputDirectory);
        URL url2 = PlaybackSimulatorTest.class.getResource(inputDirectory2);
        if (url == null || url2 == null) {
            System.out.println("Null URL.");
            System.out.println("Classpath is: " + System.getProperty("java.class.path"));
        }
        try {
            input = new File(url.toURI());
            input2 = new File(url2.toURI());
            playsim = new PlaybackSimulator(input);
        }
        catch (Exception e) {
            fail("Couldn't create directory File.");
        }
    }

    @After
    public void takeDown() {}

    @Test
    public void testPlaybackSimulatorFileFile() {
        URL url = PlaybackSimulatorTest.class.getResource(inputDirectoryStatic);
        URL url2 = PlaybackSimulatorTest.class.getResource(inputDirectoryStep);
        try {
            File staticData = new File(url.toURI());
            File stepData = new File(url2.toURI());
            PlaybackSimulator ps = new PlaybackSimulator(staticData, stepData);
        }
        catch (Exception e) {
            fail("Did not expect to throw exception but did.");
        }
    }

    @Test
    public void testPlaybackSimulatorFile() {
        try {
            PlaybackSimulator ps = new PlaybackSimulator(input2);
            fail("Expected to throw exception but did not.");
        }
        catch (Exception e) {

        }
    }

    @Test
    public void testPlaybackSimulatorFile2() {
        try {
            PlaybackSimulator ps = new PlaybackSimulator(input);
        }
        catch (Exception e) {
            fail("Did not expect to throw exception but did.");
        }
    }

    @Test
    public void testPlaybackSimulatorString() {
        URL url = PlaybackSimulatorTest.class.getResource(inputDirectory);
        try {
            File f = new File(url.toURI());
            PlaybackSimulator ps = new PlaybackSimulator(f.getPath());
        }
        catch (Exception e) {
            fail("Did not expect to throw exception but did.");
        }
    }

    @Test
    public void testAddSignalCommand() {
        playsim.addSignalCommand(null);
    }

    @Test
    public void testAddVehicleCommand() {
        playsim.addVehicleCommand(null);
    }

    @Test
    public void testClose() {
        playsim.close();
    }

    @Test
    public void testGetStaticData() {
        playsim.getStaticData();
    }

    @Test
    public void testGetStepData() {
        playsim.getStepData(7);
    }
}
