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
package org.etexascode.webapp.ejb;

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.CVAppFactory;
import org.etexascode.devicedata.IAppName;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.webapp.datamodel.application.ApplicationParameterProfile;
import org.etexascode.webapp.datamodel.application.JarApplicationProfile;
import org.etexascode.webapp.datamodel.device.DeviceType;

/**
 * Provides utility methods to process application uploads.
 * 
 * @author jconnelly
 * @author jrutherford
 * @author emyers
 */
public final class ApplicationProcessor {

    /**
     * Extracts a list of applications from bytes of file data.
     * 
     * @param fileData The bytes of file data.
     * @return A list of the extracted applications.
     * @throws IOException If an error occurs during the extraction.
     */
    public static List<JarApplicationProfile> extractApplications(byte[] fileData) throws IOException {

        List<byte[]> fileList = new ArrayList<byte[]>();
        fileList.add(fileData);

        Map<String, Class<?>> classMap = CVAppFactory.loadJars(fileList, ApplicationProcessor.class.getClassLoader());

        return ApplicationProcessor.extractApplications(classMap);
    }

    /**
     * Extracts a list of applications from a map of application classes.
     * 
     * @param classMap The map of application classes.
     * @return A list of the extracted applications.
     */
    private static List<JarApplicationProfile> extractApplications(Map<String, Class<?>> classMap) {

        List<JarApplicationProfile> applications = new ArrayList<JarApplicationProfile>();

        for (Class<?> classType : classMap.values()) {

            Class<?> currentType = classType;
            JarApplicationProfile application = new JarApplicationProfile();

            while (currentType != null) {

                for (Field field : currentType.getDeclaredFields()) {
                    for (Annotation annotation : field.getAnnotations()) {

                        if (annotation instanceof AppConfigProperty) {

                            ApplicationParameterProfile parameter = new ApplicationParameterProfile();
                            parameter.setName(field.getName());
                            parameter.setDescription(((AppConfigProperty)annotation).description());
                            parameter.setDefaultValue(((AppConfigProperty)annotation).value());
                            parameter.setDisplayName(((AppConfigProperty)annotation).displayName());
                            application.getParameterProfiles().add(parameter);
                        }
                    }
                }

                currentType = currentType.getSuperclass();
            }

            try {

                IConnectedVehicleApp<?> temp = ((IConnectedVehicleApp<?>)classType.newInstance());
                application.setName((temp instanceof IAppName) ? ((IAppName)temp).getAppName() : temp.getClass().getSimpleName());
                application.setSourceName(application.getName());
            }
            catch (IllegalAccessException exception) {

                throw new AssertionError(String.format("The fields for \"%s\" could not be accessed", classType.getName()));
            }
            catch (InstantiationException exception) {

                throw new AssertionError(String.format("\"%s\" could not be instantiated", classType.getName()));
            }

            application.setDeviceType(DeviceType.valueOfApplicationClass(classType));
            applications.add(application);
        }

        return applications;
    }

    /**
     * Extracts bytes of file data from the specified file path.
     * 
     * @param filePath The file path.
     * @return The bytes of data extracted from the file.
     * @throws IOException If an error occurs during the extraction.
     */
    public static byte[] extractJarData(Path filePath) throws IOException {

        byte[] retBytes = null;

        try (FileInputStream inputStream = new FileInputStream(filePath.toFile());
                ByteArrayOutputStream outputStream = new ByteArrayOutputStream();) {

            byte[] inputBuffer = new byte[1024];
            int bytesRead = inputStream.read(inputBuffer);
            while (bytesRead > 0) {
                outputStream.write(inputBuffer, 0, bytesRead);
                bytesRead = inputStream.read(inputBuffer);
            }

            retBytes = outputStream.toByteArray();
        }

        return retBytes;
    }

    /* prevents instantiation */
    private ApplicationProcessor() {}
}