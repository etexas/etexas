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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.lang3.CharEncoding;
import org.etexascode.CoberturaIgnore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A set of utility methods useful for running eTEXAS.
 * 
 * @author bbadillo
 */
public class RunnerUtils {

    /**
     * The logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(RunnerUtils.class);

    /**
     * Time in milliseconds for the output thread to sleep.
     */
    private static final int SLEEP_TIME = 1000;

    static public void writeSimpro(File projDir, String projName, String sysDatDir) {

        OutputStreamWriter fWriter = null;
        File simproFile = new File(projDir, "simpro.par");

        try {
            fWriter = new OutputStreamWriter(new FileOutputStream(simproFile), CharEncoding.UTF_8);
            fWriter.write("I=" + projName + "_sim\n");
            fWriter.write("L=" + projName + "_simplst.txt\n");
            fWriter.write("STA=" + projName + "_simstat\n");
            fWriter.write("T8=" + projName + "_fort8\n");
            fWriter.write("T9=" + projName + "_fort9\n");
            fWriter.write("PVA=" + projName + "_posdat\n");
            fWriter.write("ERR=" + projName + "_simerr.txt\n");
            fWriter.write("SYS_DAT=" + sysDatDir + File.separator + "\n");
            fWriter.write("SSAM=" + projName + "_ssam.trj\n");
            fWriter.write("PAUSEND=" + "NO\n");
            fWriter.close();
        }
        catch (IOException ex) {
            LOGGER.debug(ex.toString());
        }
        finally {
            try {
                if (fWriter != null) {
                    fWriter.close();
                    fWriter = null;
                }
            }
            catch (IOException ex) {
                LOGGER.debug(ex.toString());
            }
        }
    }

    static public String getProjectNameFromParFile(File projDir) throws IOException {
        File[] projFiles = projDir.listFiles(new FilenameFilter() {

            @Override
            public boolean accept(File dir, String name) {
                if (name.endsWith("_gdvdata") || name.endsWith("_simdata")) {
                    return true;
                }
                return false;
            }
        });

        if (projFiles != null && projFiles.length > 0) {
            String name = projFiles[0].getName();
            int index = name.lastIndexOf("_");
            if (index >= 0) {
                return name.substring(0, index);
            }
        }

        throw new IOException("No gdvdata or simdata project files found. Could not derive project name.");
    }

    static public TexasCommand setUpdvPro(String sysDatDir, String projName, String libPath) {
        LinkedList<String> retList = new LinkedList<String>();
        File exeFile = new File(libPath, "dvpro.exe");
        retList.add(exeFile.getPath());
        retList.add("I+" + projName + "_gdv ");
        retList.add("L+" + projName + "_dvplist.txt ");
        retList.add("T9+" + projName + "_fort9 ");
        retList.add("SYS_DAT+" + sysDatDir);
        return new TexasCommand(retList);
    }

    static public TexasCommand setUpdvPro(String sysDatDir, String projName, String libPath, int numRep, int jseed) {
        String rep = Integer.toString(numRep);
        String seed = String.format("%08d", jseed);
        TexasCommand dvProCommand = setUpdvPro(sysDatDir, projName, libPath);
        dvProCommand.getCommandList().add("REP+" + rep);
        dvProCommand.getCommandList().add("JSEED+" + seed);
        return dvProCommand;
    }

    static public TexasCommand setUpGDVConv(String sysDatDir, String projName, String libPath) {
        LinkedList<String> retList = new LinkedList<String>();
        File exeFile = new File(libPath, "gdvconv.exe");
        retList.add(exeFile.getPath());
        retList.add("PRE+" + projName + "_gdvdata ");
        retList.add("C+" + projName + "_gdv ");
        retList.add("SYS_DAT+" + sysDatDir);
        return new TexasCommand(retList);
    }

    static public TexasCommand setUpGeoPro(String sysDatDir, String projName, String libPath) {
        LinkedList<String> retList = new LinkedList<String>();
        File exeFile = new File(libPath, "geopro.exe");
        retList.add(exeFile.getPath());
        retList.add("I+" + projName + "_gdv ");
        retList.add("L+" + projName + "_geolist.txt ");
        retList.add("T8+" + projName + "_fort8 ");
        retList.add("PLOT+" + projName + "_geoplot ");
        retList.add("SYS_DAT+" + sysDatDir);
        return new TexasCommand(retList);
    }

    static public TexasCommand setUpSimConv(String sysDatDir, String projName, String libPath) {
        LinkedList<String> retList = new LinkedList<String>();
        File exeFile = new File(libPath, "simconv.exe");
        retList.add(exeFile.getPath());
        retList.add("PRE+" + projName + "_simdata ");
        retList.add("C+" + projName + "_sim");
        retList.add("SYS_DAT+" + sysDatDir);
        return new TexasCommand(retList);
    }

    @CoberturaIgnore
    static public void runTexasExecutable(File projDir, TexasCommand command) throws IOException {
        List<String> commandList = command.getCommandList();
        ProcessBuilder processBuilder = new ProcessBuilder(commandList);
        processBuilder.directory(projDir);
        processBuilder.redirectErrorStream(true);

        Process process = processBuilder.start();

        InputStreamReader inputStreamReader = new InputStreamReader(process.getInputStream(), "ASCII");

        Stoppable stoppable = new Stoppable(inputStreamReader);

        Thread outputThread = new Thread(stoppable, "build-output-thread");
        outputThread.start();

        // Wait for the process to finish.
        try {
            process.waitFor();
            stoppable.setStopping();
            outputThread.join(SLEEP_TIME * 2);
            if (outputThread.isAlive()) {
                outputThread.interrupt();
            }
            outputThread = null;

            try {
                process.getOutputStream().close();
            }
            catch (IOException ex) {
                LOGGER.debug(null, ex);
            }
            finally {
                process.destroy();
                process = null;
            }
        }
        catch (InterruptedException ex) {
            LOGGER.debug(null, ex);
        }
    }

    private static class Stoppable implements Runnable {

        private boolean stopping = true;

        private BufferedReader reader;

        public Stoppable(InputStreamReader inputStreamReader) {
            reader = new BufferedReader(inputStreamReader);
        }

        synchronized public boolean isStopping() {
            return stopping;
        }

        @Override
        public void run() {
            try {
                String readLine;
                while (isStopping()) {
                    while (reader != null && !reader.ready()) {
                        try {
                            Thread.sleep(SLEEP_TIME);
                        }
                        catch (InterruptedException ex) {
                            LOGGER.trace("eTEXAS output thread interrupted.");
                        }
                    }
                    if (reader != null) {
                        readLine = reader.readLine();
                        if (readLine != null && readLine.trim().length() > 0) {
                            LOGGER.info(readLine);
                        }
                    }
                }
            }
            catch (IOException ex) {
                LOGGER.debug(null, ex);
            }
            finally {
                try {
                    if (reader != null) {
                        reader.close();
                        reader = null;
                    }
                }
                catch (IOException ex) {
                    LOGGER.debug(null, ex);
                }
            }
        }

        synchronized public void setStopping() {
            stopping = false;
        }
    }

    static public class TexasCommand {

        private List<String> commandList;

        private TexasCommand() {}

        private TexasCommand(List<String> commandList) {
            this.commandList = commandList;
        }

        private List<String> getCommandList() {
            return commandList;
        }
    }
}