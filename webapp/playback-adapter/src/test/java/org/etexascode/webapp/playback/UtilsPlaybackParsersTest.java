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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import org.junit.Test;

public class UtilsPlaybackParsersTest {

    final double TOLERANCE = .00005;

    String double1 = "label:2.5";

    String double2 = "foo:6.25";

    String integer1 = "label:1";

    String integer2 = "label:10";

    @Test
    public void testParseDouble() {
        assertEquals(2.5, UtilsPlaybackParsers.parseDouble(double1), TOLERANCE);
        assertEquals(6.25, UtilsPlaybackParsers.parseDouble(double2), TOLERANCE);
    }

    @Test
    public void testParseInt() {
        assertEquals(1, UtilsPlaybackParsers.parseInt(integer1));
        assertEquals(10, UtilsPlaybackParsers.parseInt(integer2));
    }

    @Test
    public void testConstructor() {
        try {
            UtilsPlaybackParsers upp = new UtilsPlaybackParsers();
        }
        catch (Exception e) {
            fail("Did not expect to throw exception, but did.");
        }
    }

}
