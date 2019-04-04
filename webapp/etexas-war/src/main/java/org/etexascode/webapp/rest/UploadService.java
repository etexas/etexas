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
package org.etexascode.webapp.rest;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;
import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.MediaType;

import org.etexascode.webapp.cdi.CurrentUser;
import org.etexascode.webapp.datamodel.Simulation;
import org.etexascode.webapp.datamodel.SimulationType;
import org.etexascode.webapp.datamodel.application.ApplicationParameterProfile;
import org.etexascode.webapp.datamodel.application.JarApplicationProfile;
import org.etexascode.webapp.ejb.ApplicationProfileManager;
import org.etexascode.webapp.ejb.SimulationManager;
import org.etexascode.webapp.exception.UploadException;
import org.etexascode.webapp.exception.WebAppException;
import org.etexascode.webapp.rest.validation.ApplicationProfileFileNameValidator;
import org.etexascode.webapp.rest.validation.SimulationNameValidator;
import org.etexascode.webapp.rest.validation.SimulationTypeValidator;
import org.etexascode.webapp.rest.validation.XCoordinateValidator;
import org.etexascode.webapp.rest.validation.YCoordinateValidator;
import org.etexascode.webapp.util.AbstractRequestProcessor;

/**
 * The REST service for upload operation requests.
 * 
 * @author bbadillo
 * @author emyers
 */
public class UploadService extends HttpServlet {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The application transaction manager. */
    @Inject
    private ApplicationProfileManager applicationManager;

    /** The current user. */
    @Inject
    private CurrentUser user;

    /** The simulation transaction manager. */
    @Inject
    private SimulationManager simulationManager;

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {

        UploadProcessor processor = new UploadProcessor();
        processor.processRequest(request);

        if (processor.fileData == null || processor.fileData.length == 0) {

            throw new UploadException("File Upload Failure", "No data could be extracted from the uploaded file.", HttpServletResponse.SC_CONFLICT);
        }

        if (processor.partMap.containsKey("applicationFileName")) {

            String exceptionTitle = "Upload JAR Application Profiles Failure";

            try {

                new ApplicationProfileFileNameValidator(processor.partMap.get("applicationFileName").trim()).validate();
            }
            catch (WebAppException exception) {

                throw new UploadException(exceptionTitle, exception.getMessage(), HttpServletResponse.SC_CONFLICT);
            }

            String fileName = processor.partMap.get("applicationFileName").trim();
            List<JarApplicationProfile> jarApplicationProfiles;

            try {

                jarApplicationProfiles = applicationManager.addJarApplicationProfiles(user.getId(), fileName, processor.fileData, false);
            }
            catch (WebAppException exception) {

                throw new UploadException(exceptionTitle, exception.getMessage(), exception.getStatus().getStatusCode());
            }

            JsonArrayBuilder applicationProfileBuilder = Json.createArrayBuilder();

            for (JarApplicationProfile jarApplicationProfile : jarApplicationProfiles) {

                JsonArrayBuilder parameterProfileBuilder = Json.createArrayBuilder();

                for (ApplicationParameterProfile parameterProfile : jarApplicationProfile.getParameterProfiles()) {

                    parameterProfileBuilder.add(Json.createObjectBuilder()
                            .add("name", parameterProfile.getName())
                            .add("defaultValue", parameterProfile.getDefaultValue()).build());
                }

                applicationProfileBuilder.add(Json.createObjectBuilder()
                        .add("id", jarApplicationProfile.getId())
                        .add("name", jarApplicationProfile.getName())
                        .addNull("type")
                        .add("fileName", fileName)
                        .add("isEmbedded", jarApplicationProfile.isEmbedded())
                        .add("deviceType", jarApplicationProfile.getDeviceType().name())
                        .add("parameterProfiles", parameterProfileBuilder.build()).build());
            }

            response.setStatus(HttpServletResponse.SC_CREATED);
            response.setContentType(MediaType.APPLICATION_JSON);
            response.getWriter().write(applicationProfileBuilder.build().toString());
        }
        else if (processor.partMap.containsKey("simulationName")) {

            Double x;
            Double y;
            Long compositeId;
            String simulationName = processor.partMap.get("simulationName").trim();
            String simulationTypeString = processor.partMap.get("simulationType").trim();
            String exceptionTitle = "Upload Simulation Failure";

            try {

                x = Double.parseDouble(processor.partMap.get("x"));
                y = Double.parseDouble(processor.partMap.get("y"));
                compositeId = Long.parseLong(processor.partMap.get("compositeId"));
            }
            catch (NumberFormatException exception) {

                throw new UploadException(exceptionTitle, exception.getMessage(), HttpServletResponse.SC_BAD_REQUEST);
            }

            try {

                new SimulationNameValidator(simulationName).validate();
                new XCoordinateValidator(x).validate();
                new YCoordinateValidator(y).validate();
                new SimulationTypeValidator(simulationTypeString).validate();
            }
            catch (WebAppException exception) {

                throw new UploadException(exceptionTitle, exception.getMessage(), HttpServletResponse.SC_CONFLICT);
            }

            Simulation simulation;
            byte[] data = processor.fileData;
            SimulationType simulationType = SimulationType.valueOf(simulationTypeString);

            try {

                simulation = simulationManager.addSimulation(user.getId(), compositeId, simulationName, x, y, simulationType, "Uploaded File", data);
            }
            catch (WebAppException exception) {

                throw new UploadException(exceptionTitle, exception.getMessage(), exception.getStatus().getStatusCode());
            }

            JsonObject simulationObject = Json.createObjectBuilder()
                    .add("id", simulation.getId())
                    .add("name", simulation.getName())
                    .add("x", simulation.getX())
                    .add("y", simulation.getY())
                    .add("type", simulation.getType().name())
                    .add("source", simulation.getSource())
                    .build();

            response.setStatus(HttpServletResponse.SC_CREATED);
            response.setContentType(MediaType.APPLICATION_JSON);
            response.getWriter().write(simulationObject.toString());
        }
    }

    /**
     * The processor for multiple part file uploads.
     * 
     * @author ablatt
     * @author bbadillo
     * @author emyers
     */
    private static class UploadProcessor extends AbstractRequestProcessor {

        /** The number of bytes in a megabyte. */
        private static final int MEGABYTE = 1048576;

        /** The file data for this upload. */
        private byte[] fileData;

        /** The map of part values for this upload. */
        private Map<String, String> partMap = new HashMap<String, String>();

        @Override
        protected void processFilePart(String name, InputStream inputStream) throws IOException {

            byte[] buffer = new byte[8192];
            ByteArrayOutputStream outputStream = new ByteArrayOutputStream(UploadProcessor.MEGABYTE);

            int length = inputStream.read(buffer);

            while (length > 0) {

                outputStream.write(buffer, 0, length);
                length = inputStream.read(buffer);
            }

            outputStream.close();
            fileData = outputStream.toByteArray();
        }

        @Override
        protected void processPart(String name, String value) {

            partMap.put(name, value);
        }
    }
}