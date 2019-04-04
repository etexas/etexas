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

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Factory class that loads services that are Connected Vehicle Apps.
 * 
 * @author bbadillo
 * @author emyers
 */
public class CVAppFactory {

    /**
     * Static logger for convenience.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(CVAppFactory.class);

    /**
     * Extracts the App class files from the zip file.
     * 
     * @param archive The zip archive.
     * @return Map of filenames and class file bytes.
     * @throws IOException If there's a problem with processing the file.
     */
    public static Map<String, byte[]> extractAppsFromZip(byte archive[]) throws IOException {
        int bufferSize = 8192;
        ByteArrayInputStream bis = new ByteArrayInputStream(archive);
        BufferedInputStream bufferedInputStream = new BufferedInputStream(bis, bufferSize);
        ZipInputStream zis = new ZipInputStream(bufferedInputStream);
        ZipEntry entry = zis.getNextEntry();
        Map<String, byte[]> classes = new HashMap<String, byte[]>();

        // Loop through each file.
        while (entry != null) {
            // Ignore non class files and secondary classes.
            if (entry.getName().endsWith(".class")) {// ) &&
                                                     // !entry.getName().contains("$"))
                                                     // {
                String fileName = entry.getName();
                fileName = fileName.substring(fileName.lastIndexOf("/") + 1);

                // Read the file contents into a list of bytes
                // since the file size is unknown.
                int count = 0;
                byte buffer[] = new byte[bufferSize];
                List<Byte> fileBuffer = new ArrayList<Byte>();
                while ((count = zis.read(buffer, 0, bufferSize)) != -1) {
                    for (int i = 0; i < count; i++) {
                        fileBuffer.add(buffer[i]);
                    }
                }

                // Copy the list into a byte array.
                byte data[] = new byte[fileBuffer.size()];
                for (int i = 0; i < fileBuffer.size(); i++) {
                    data[i] = fileBuffer.get(i);
                }
                classes.put(fileName, data);
                LOGGER.debug("Extracted File: {}", entry.getName());
            }
            entry = zis.getNextEntry();
        }
        return classes;
    }

    /**
     * Returns a map of application classes extracted from a jar file.
     * 
     * @param jars The jar file (list of byte arrays).
     * @param parentClassLoader The parent class loader.
     * @return Map of application IDs (or class names) and application classes.
     * @throws IOException If there's a problem with extracting the apps from the zip.
     */
    public static Map<String, Class<?>> loadJars(List<byte[]> jars, final ClassLoader parentClassLoader) throws IOException {
        List<Map<String, byte[]>> appsByJar = new LinkedList<Map<String, byte[]>>();
        for (byte[] jar : jars) {
            appsByJar.add(extractAppsFromZip(jar));
        }

        final Map<String, byte[]> apps = new HashMap<String, byte[]>();
        for (Map<String, byte[]> jarApps : appsByJar) {
            for (Entry<String, byte[]> entry : jarApps.entrySet()) {
                if (!apps.containsKey(entry.getKey())) {
                    apps.put(entry.getKey(), entry.getValue());
                }
            }
        }

        AppsClassloader loader = AccessController.doPrivileged(new PrivilegedAction<AppsClassloader>() {

            @Override
            public AppsClassloader run() {
                return new AppsClassloader(parentClassLoader, apps);
            }
        });

        Map<String, Class<?>> ret = new HashMap<String, Class<?>>();

        for (Entry<String, byte[]> entry : apps.entrySet()) {
            Class<?> clazz;

            try {
                clazz = loader.loadClass(entry.getKey());

                if (IConnectedVehicleApp.class.isAssignableFrom(clazz)) {
                    IConnectedVehicleApp<?> tempapp = ((IConnectedVehicleApp<?>)clazz.newInstance());

                    if (tempapp instanceof IAppName) {
                        ret.put(((IAppName)tempapp).getAppName(), clazz);
                    }
                    else {
                        ret.put(tempapp.getClass().getSimpleName(), clazz);
                    }
                }
            }
            catch (Throwable t) {
                LOGGER.error("Error while loading classes", t);
            }
        }

        return ret;
    }

    /**
     * This is an extended classloader to allow loading of byte arrays.
     * 
     * @author jrutherford
     * @author bbadillo
     */
    public static class AppsClassloader extends ClassLoader {

        /**
         * A map of byte arrays representing classes keyed by simple class names.
         */
        private final Map<String, byte[]> classes;

        /**
         * This constructor is used to set the parent ClassLoader.
         * 
         * @param parent Parent classloader.
         * @param classes A map of byte arrays representing classes keyed by simple class names.
         */
        public AppsClassloader(ClassLoader parent, Map<String, byte[]> classes) {
            super(parent);
            this.classes = classes;
        }

        @Override
        public Class<?> findClass(String name) throws ClassNotFoundException {
            byte clazz[] = classes.get(name);
            if (clazz == null || clazz.length <= 0) {
                throw new ClassNotFoundException(String.format("%s could not find %s", AppsClassloader.class.getName(), name));
            }
            return this.defineClass(null, clazz, 0, clazz.length);
        }
    }
}
