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

import static org.junit.Assert.fail;

import java.io.File;

import org.junit.Before;
import org.junit.Test;
import org.powermock.api.mockito.PowerMockito;

public class SimProcessRunnerTest {

    private String uuid;

    private int port;

    private String libPath;

    private File projDir;

    private String sysDatDir;

    private String javaCommand;

    private File mockLibPathFile;

    private ProcessBuilder mockPB;

    @Before
    public void setUp() {
        uuid = "uuid";
        port = 9001;
        libPath = "libPath";
        projDir = PowerMockito.mock(File.class);
        sysDatDir = "sysDatDir";
        javaCommand = "javaCommand";

        mockLibPathFile = PowerMockito.mock(File.class);
    }

    @Test
    public void testSimProcessRunner() {
        try {
            PowerMockito.whenNew(File.class).withArguments(libPath).thenReturn(mockLibPathFile);
            PowerMockito.when(mockLibPathFile.getCanonicalPath()).thenReturn("mockLibPathFileCanonicalPath");
            PowerMockito.whenNew(ProcessBuilder.class).withAnyArguments().thenReturn(mockPB);

            new SimProcessRunner(uuid, port, libPath, projDir, sysDatDir, javaCommand, null);
            new SimProcessRunner(uuid, port, libPath, projDir, sysDatDir, null, null);
            new SimProcessRunner(uuid, port, libPath, projDir, sysDatDir, "", null);
        }
        catch (Exception e) {
            e.printStackTrace();
            fail("Did not expect to throw exception but did.");
        }
    }

    // NOTE: janway - Decided not to test this because...
    // 1) It'd be fairly tricky due to the "new Thread"
    // 2) There's not much logic
    // 3) It's function isn't especially critical -- it reads output and discards it (leaving the
    // output unread can cause problems somehow).
    // 4) Integration tests would be more appropriate.
    // @Test
    // public void testStart() {
    // fail("Not yet implemented");
    // }

    // NOTE: janway - Decided not to test this. See above.
    // @Test
    // public void testStop() {
    // fail("Not yet implemented");
    // }

    // NOTE: janway - Decided not to test this because...
    // 1) There's not much logic.
    // 2) Integration tests would be more appropriate.
    // 3) Trying to mock the static Registry lookup caused problems with the constructor test.
    // See https://code.google.com/p/powermock/issues/detail?id=351
    // @Test
    // public void testConnectToSimDriver() {
    // fail("Not yet implemented");
    // }
    //

}
