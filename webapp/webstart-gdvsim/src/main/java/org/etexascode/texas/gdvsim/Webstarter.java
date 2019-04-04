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

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.ws.rs.core.UriBuilder;

import org.apache.commons.io.output.ByteArrayOutputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.WebResource;

/**
 * A Web Start launcher for GDVSIM
 * 
 * @author bbadillo
 */
public class Webstarter {

    /**
     * Logger for convenience.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(Webstarter.class);

    /**
     * Size of the byte buffer to use for stream transfers.
     */
    private static final int BUFFER_SIZE = 4096;

    /**
     * @param args the command line arguments
     * @throws IOException If an IO exception occurs.
     */
    public static void main(String[] args) throws IOException {

        /*
         * Set the Nimbus look and feel
         */
        boolean nimbus = false;
        try {
            for (UIManager.LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
                if ("Nimbus".equals(info.getName())) {
                    UIManager.setLookAndFeel(info.getClassName());
                    nimbus = true;
                    break;
                }
            }
        }
        catch (ClassNotFoundException ex) {
            LOGGER.debug(ex.toString());
        }
        catch (InstantiationException ex) {
            LOGGER.debug(ex.toString());
        }
        catch (IllegalAccessException ex) {
            LOGGER.debug(ex.toString());

        }
        catch (UnsupportedLookAndFeelException ex) {
            LOGGER.debug(ex.toString());

        }

        if (!nimbus) {
            try {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            }
            catch (ClassNotFoundException ex) {
                LOGGER.debug(ex.toString());

            }
            catch (InstantiationException ex) {
                LOGGER.debug(ex.toString());

            }
            catch (IllegalAccessException ex) {
                LOGGER.debug(ex.toString());

            }
            catch (UnsupportedLookAndFeelException ex) {
                LOGGER.debug(ex.toString());

            }
        }

        Webstarter webstarter = new Webstarter(args[0]);
        webstarter.retrieveProjectFiles();

        String params[] = new String[] { webstarter.getGdvdata().toString(), webstarter.getSimdata().toString() };

        // This simply needs to be created to be shown.
        new OverriddenGDVSIM(webstarter, params[0], params[1]);
    }

    /**
     * Jersey REST Client
     */
    private Client restClient = null;

    /**
     * Jersey resource used to consume REST services
     */
    private WebResource resource = null;

    private File downloadedFile;

    private File simdata;

    private File gdvdata;

    public Webstarter(String endpoint) {
        LOGGER.info("Launcher endpoint: {}", endpoint);

        restClient = Client.create();
        resource = restClient.resource(UriBuilder.fromUri(endpoint).build());
    }

    public File getGdvdata() {
        return gdvdata;
    }

    public File getSimdata() {
        return simdata;
    }

    void retrieveProjectFiles() throws ZipException, FileNotFoundException, IOException {
        String projectName = null;
        downloadedFile = resource.get(File.class);
        ZipFile zipFile = new ZipFile(downloadedFile);
        Enumeration<? extends ZipEntry> entries = zipFile.entries();
        while (entries.hasMoreElements()) {
            ZipEntry entry = entries.nextElement();
            InputStream zis = zipFile.getInputStream(entry);

            int count;
            byte buffer[] = new byte[BUFFER_SIZE];
            String filename = entry.getName();
            File newFile = null;

            // Check the name of the file and extract the project name
            // if it a design file.
            int index = filename.lastIndexOf("_gdvdata");
            if (index >= 0) {
                gdvdata = newFile = Paths.get(System.getProperty("java.io.tmpdir"), filename).toFile();
                if (projectName == null) {
                    projectName = filename.substring(0, index);
                }
            }
            else {
                index = filename.lastIndexOf("_simdata");
                if (index >= 0) {
                    simdata = newFile = Paths.get(System.getProperty("java.io.tmpdir"), filename).toFile();
                    if (projectName == null) {
                        projectName = filename.substring(0, index);
                    }
                }
            }

            if (newFile == null) {

                continue;
            }
            else if (newFile.exists()) {

                Files.delete(newFile.toPath());
            }

            LOGGER.info("Launcher file: {}", newFile.getAbsolutePath());

            // Output the file to disk.
            FileOutputStream fos;
            try {
                fos = new FileOutputStream(newFile);
            }
            catch (FileNotFoundException ex) {
                LOGGER.debug(ex.toString());
                throw ex;
            }

            BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(fos, BUFFER_SIZE);
            try {
                while ((count = zis.read(buffer, 0, BUFFER_SIZE)) != -1) {
                    bufferedOutputStream.write(buffer, 0, count);
                }
                bufferedOutputStream.flush();
                bufferedOutputStream.close();
            }
            catch (IOException ex) {
                LOGGER.debug(ex.toString());
                throw ex;
            }
            finally {
                try {
                    bufferedOutputStream.close();
                }
                catch (IOException ex) {
                    LOGGER.debug(ex.toString());
                }
            }
        }
        zipFile.close();
    }

    void sendProjectFiles() throws FileNotFoundException, IOException {
        LOGGER.info("Downloaded file has been deleted: {}", downloadedFile.getAbsolutePath());

        int count;
        byte buffer[] = new byte[BUFFER_SIZE];

        // Zip up the project files to a byte array.
        // Important! Using Apache Commons IO ByteArrayOutputStream because
        // of the way that it grows the backing buffer without copying and
        // garbage collecting. This is necesary because the resulting size of
        // the zip is unknown at this time.
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream(BUFFER_SIZE);
        ZipOutputStream zos = new ZipOutputStream(byteArrayOutputStream);
        LOGGER.info("Zip stream opened.");
        try {
            zos.putNextEntry(new ZipEntry(gdvdata.getName()));
            FileInputStream gdvInput = new FileInputStream(gdvdata);
            try {
                while ((count = gdvInput.read(buffer, 0, BUFFER_SIZE)) != -1) {
                    zos.write(buffer, 0, count);
                }
            }
            finally {
                gdvInput.close();
            }
            LOGGER.info("Wrote gdvdata.");

            zos.putNextEntry(new ZipEntry(simdata.getName()));
            FileInputStream simInput = new FileInputStream(simdata);
            try {
                while ((count = simInput.read(buffer, 0, BUFFER_SIZE)) != -1) {
                    zos.write(buffer, 0, count);
                }
            }
            finally {
                simInput.close();
            }
            LOGGER.info("Wrote simdata.");

        }
        finally {
            try {
                zos.close();
            }
            finally {
                if (zos != null) {
                    try {
                        zos.close();
                    }
                    catch (IOException ex) {
                        LOGGER.debug(ex.toString());
                    }
                }

            }
        }

        // Send the zipped file to the server
        LOGGER.info("Sending file: {}", downloadedFile.getAbsolutePath());
        resource.put(byteArrayOutputStream.toByteArray());
        LOGGER.info("File sent: {}", downloadedFile.getAbsolutePath());
    }
}
