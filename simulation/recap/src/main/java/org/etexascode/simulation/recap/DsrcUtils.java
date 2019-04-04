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
package org.etexascode.simulation.recap;

import java.io.BufferedReader;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Defines utility methods to convert DSRC message log formats and constants for DSRC message
 * processing.
 * 
 * @author emyers
 */
public class DsrcUtils {

    /**
     * The PSID for all BSMs.
     */
    public static final String BSM_PSID = "20";

    /**
     * The prefix before BSM content.
     */
    public static final String BSM_PREFIX = "8001028126";

    /**
     * The latitude error indicator.
     */
    public static final double LAT_ERROR = 90.0000001;

    /**
     * The longitude error indicator.
     */
    public static final double LON_ERROR = 180.0000001;

    /*
     * Private to prevent direct instantiation.
     */
    private DsrcUtils() {}

    /**
     * Creates a PCAP file at the specified target file path that contains the merged (chronological
     * order) data from each PCAP file in the given source directory. The method uses the
     * <code>mergecap</code> command included in the <b>Wireshark</b> program, and assumes that the
     * path to the executable is added to the path environment variable.
     * 
     * @param sourcePath the path to the source PCAP file directory
     * @param targetPath the path to the merged PCAP file
     * @throws IOException if an I/O error occurs while merging PCAP files
     * @throws InterruptedException if the merge thread is interrupted
     * @throws IllegalArgumentException if the given target path does not have a valid
     *         <code>.pcap</code> extension, or no PCAP files are found in the given source
     *         directory
     */
    public static void mergePcapFiles(Path sourcePath, Path targetPath) throws IOException, InterruptedException {

        // if an invalid target file name is given
        if (!targetPath.toString().endsWith(".pcap")) {

            // throw an IllegalArgumentException to indicate the error
            throw new IllegalArgumentException(
                    "the target path does not specify a .pcap file");
        }

        // get the list of PCAP files in the source file directory
        String[] sourceFiles = sourcePath.toFile().list(new FilenameFilter() {

            @Override
            public boolean accept(File dir, String name) {

                return name.endsWith(".pcap");
            }
        });

        // if no PCAP files were present
        if (sourceFiles == null || sourceFiles.length == 0) {

            // throw an IllegalArgumentException to indicate the error
            throw new IllegalArgumentException("no PCAP data was found");
        }

        // build the command to merge the source PCAP files
        StringBuilder commandBuilder = new StringBuilder(String.format(
                "mergecap -w \"%s\"", targetPath.toString()));

        for (String sourceFile : sourceFiles) {
            commandBuilder.append(String.format(" \"%s\"", Paths.get(
                    sourcePath.toString(), sourceFile)));
        }

        // execute the command to merge the source PCAP files
        Runtime.getRuntime().exec(commandBuilder.toString()).waitFor();
    }

    /**
     * Creates a new PDML file at the specified target file path that contains the data from the
     * PCAP file at the given source path. The method used the <code>tshark</code> command included
     * in the <b>Wireshark</b> program, and assumes that the path to the executable is added to the
     * path environment variable.
     * 
     * @param sourcePath the path to the source PCAP file
     * @param targetPath the path to the resulting PDML file
     * @throws IOException if an I/O error occurs during file conversion
     * @throws InterruptedException if the conversion thread is interrupted
     * @throws IllegalArgumentException if the given source path does not have a valid
     *         <code>.pcap</code> extension, or the given target path does not have a valid
     *         <code>.pdml</code> extension
     */
    public static void pcapToPdml(Path sourcePath, Path targetPath) throws IOException, InterruptedException {

        // if an invalid source file name is given
        if (!sourcePath.toString().endsWith(".pcap")) {

            // throw an IllegalArgumentException to indicate the error
            throw new IllegalArgumentException(
                    "the given source file is not a valid .pcap file");
        }

        // if an invalid target file name is given
        if (!targetPath.toString().endsWith(".pdml")) {

            // throw an IllegalArgumentException to indicate the error
            throw new IllegalArgumentException(
                    "the given target file is not a valid .pdml file");
        }

        // create the command to convert the source PCAP file to PDML
        String command = String.format("tshark -r \"%s\" -T pdml", sourcePath.toString());

        // open the file to print the resulting output and execute the command
        PrintWriter pdmlWriter = new PrintWriter(targetPath.toFile());
        Process process = Runtime.getRuntime().exec(command);
        BufferedReader input = new BufferedReader(
                new InputStreamReader(process.getInputStream()));

        // write the PDML output to the opened file
        String line = input.readLine();
        while (line != null) {

            pdmlWriter.println(line);
            line = input.readLine();
        }

        process.waitFor(); // wait for the command to complete
        pdmlWriter.close(); // close the output file
    }
}