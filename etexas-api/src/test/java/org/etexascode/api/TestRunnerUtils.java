/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * * ** ************************************************************** ** *
 * * ** *                                                            * ** *
 * * ** *  COPYRIGHT (C) 2003 by The University of Texas at Austin   * ** *
 * * ** *                                                            * ** *
 * * ** * Permission is hereby granted to use, modify, copy, and     * ** *
 * * ** * distribute this software and its documentation for any     * ** *
 * * ** * purpose only without profit, provided that the above       * ** *
 * * ** * Copyright Notice appears in all copies and that both the   * ** *
 * * ** * Copyright Notice and this Permission Notice appears in     * ** *
 * * ** * every copy of supporting documentation.  No title to nor   * ** *
 * * ** * ownership of the software is transferred hereby.  The name * ** *
 * * ** * of The University of Texas at Austin shall not be used in  * ** *
 * * ** * advertising or publicity related to the distribution of    * ** *
 * * ** * the software without specific, written, prior permission.  * ** *
 * * ** * This software is provided as-delivered without expressed   * ** *
 * * ** * or implied warranty.  The University of Texas at Austin    * ** *
 * * ** * makes no representation about the suitability of this      * ** *
 * * ** * software for any purpose and accepts no responsibility for * ** *
 * * ** * its use.                                                   * ** *
 * * ** *                                                            * ** *
 * * ** ************************************************************** ** *
 * * ** *                                                            * ** *
 * * ** * This program is free software; you can redistribute it     * ** *
 * * ** * and/or modify it under the terms of the GNU General Public * ** *
 * * ** * License as published by the Free Software Foundation;      * ** *
 * * ** * either version 2 of the License, or (at your option) any   * ** *
 * * ** * later version.                                             * ** *
 * * ** *                                                            * ** *
 * * ** * This program is distributed in the hope that it will be    * ** *
 * * ** * useful, but WITHOUT ANY WARRANTY; without even the implied * ** *
 * * ** * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    * ** *
 * * ** * PURPOSE.  See the GNU General Public License for more      * ** *
 * * ** * details.                                                   * ** *
 * * ** *                                                            * ** *
 * * ** * You should have received a copy of the GNU General Public  * ** *
 * * ** * License along with this program; if not, write to the Free * ** *
 * * ** * Software Foundation, Inc., 51 Franklin Street, Fifth       * ** *
 * * ** * Floor, Boston, MA 02110-1301, USA.                         * ** *
 * * ** *                                                            * ** *
 * * ** * For more information: http://www.gnu.org/licenses/gpl.html * ** *
 * * ** *                                                            * ** *
 * * ** ************************************************************** ** *
 * #L%
 */
package org.etexascode.api;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileAttribute;

import org.etexascode.api.RunnerUtils.TexasCommand;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

/**
 * Tests for the Runner Utils class.
 * 
 * @author jrutherford
 * @author emyers
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest(RunnerUtils.class)
public class TestRunnerUtils {

    /**
     * Tests the <code>writeSimpro(File, String, String)</code> method under normal conditions.
     * 
     * @throws IOException if the test project directory creation fails
     */
    @Test
    public void testWriteSimpro() throws IOException {

        Path dirPath = Files.createTempDirectory(null, new FileAttribute<?>[0]);
        RunnerUtils.writeSimpro(dirPath.toFile(), "testProj", dirPath.toString());
        Assert.assertTrue(Paths.get(dirPath.toString(), "simpro.par").toFile().exists());
    }

    /**
     * Tests the <code>writeSimpro(File, String, String)</code> method when an exception is thrown
     * during file writing.
     * 
     * @throws Exception if the test project directory creation fails or an error occurs during
     *         object mocking
     */
    @Test
    public void testWriteSimpro2() throws Exception {

        OutputStreamWriter writerMock = Mockito.mock(OutputStreamWriter.class);
        Mockito.doThrow(new IOException()).when(writerMock).write(Mockito.anyString());
        PowerMockito.whenNew(OutputStreamWriter.class).withAnyArguments().thenReturn(writerMock);
        Path dirPath = Files.createTempDirectory(null, new FileAttribute<?>[0]);
        RunnerUtils.writeSimpro(dirPath.toFile(), "testProj", dirPath.toString());
        Assert.assertTrue(Paths.get(dirPath.toString(), "simpro.par").toFile().exists());
    }

    /**
     * Tests the <code>writeSimpro(File, String, String)</code> method when an exception is thrown
     * during file writing and file closing.
     * 
     * @throws Exception if the test project directory creation fails or an error occurs during
     *         object mocking
     */
    @Test
    public void testWriteSimpro3() throws Exception {

        OutputStreamWriter writerMock = Mockito.mock(OutputStreamWriter.class);
        Mockito.doThrow(new IOException()).when(writerMock).write(Mockito.anyString());
        Mockito.doThrow(new IOException()).when(writerMock).close();
        PowerMockito.whenNew(OutputStreamWriter.class).withAnyArguments().thenReturn(writerMock);
        Path dirPath = Files.createTempDirectory(null, new FileAttribute<?>[0]);
        RunnerUtils.writeSimpro(dirPath.toFile(), "testProj", dirPath.toString());
        Assert.assertTrue(Paths.get(dirPath.toString(), "simpro.par").toFile().exists());
    }

    @Test
    public void testGetProjectNameFromParFile() throws IOException {
        File sd = new File("aProj_simdata");
        File gd = new File("aProj_gdvdata");

        sd.createNewFile();
        gd.createNewFile();

        String projDir = sd.getAbsolutePath().substring(0, sd.getAbsolutePath().length() - 13);
        String name = RunnerUtils.getProjectNameFromParFile(new File(projDir));
        assertTrue(name.equals("aProj"));

        sd.delete();
        gd.delete();
    }

    @Test(expected = IOException.class)
    public void testGetProjectNameFromParFileException() throws IOException {

        Path tempDir = Files.createTempDirectory(null, new FileAttribute<?>[0]);
        RunnerUtils.getProjectNameFromParFile(tempDir.toFile());
    }

    @Test
    public void testSetUpApplications() {
        File f = new File("test");
        String path = f.getAbsolutePath();
        path = path.substring(0, path.length() - 4);
        TexasCommand texas1 = RunnerUtils.setUpdvPro(path, "aProj", path);
        TexasCommand texas2 = RunnerUtils.setUpdvPro(path, "aProj", path, 5, 10);
        TexasCommand texas3 = RunnerUtils.setUpGDVConv(path, "aProj", path);
        TexasCommand texas4 = RunnerUtils.setUpGeoPro(path, "aProj", path);
        TexasCommand texas5 = RunnerUtils.setUpSimConv(path, "aProj", path);
        assertTrue(texas1 != null);
        assertTrue(texas2 != null);
        assertTrue(texas3 != null);
        assertTrue(texas4 != null);
        assertTrue(texas5 != null);
    }
}
