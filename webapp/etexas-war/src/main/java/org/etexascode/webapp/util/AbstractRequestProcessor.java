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

package org.etexascode.webapp.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.LinkedList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.Part;

import org.slf4j.LoggerFactory;

/**
 * A processor that parses part information in multipart form requests.
 * 
 * @author bbadillo
 * @author emyers
 */
public abstract class AbstractRequestProcessor {

    /** The default character encoding. */
    private static final String DEFAULT_ENCODING = "UTF-8";

    /** The character encoding for this processor. */
    private String encoding;

    /**
     * Processes the parts for the specified HTTP servlet request.
     * 
     * @param request The HTTP servlet request.
     */
    public void processRequest(HttpServletRequest request) {

        if (request.getCharacterEncoding() == null) {

            try {

                request.setCharacterEncoding(AbstractRequestProcessor.DEFAULT_ENCODING);
            }
            catch (UnsupportedEncodingException exception) {

                LoggerFactory.getLogger(AbstractRequestProcessor.class).error(null, exception);
            }
        }

        this.encoding = request.getCharacterEncoding();
        List<Part> toDelete = new LinkedList<Part>();

        try {

            for (Part part : request.getParts()) {

                if (isFilePart(part)) {

                    InputStream inputStream = part.getInputStream();
                    processFilePart(part.getName(), inputStream);
                    inputStream.close();
                    toDelete.add(part);
                }
                else {

                    processPart(part.getName(), getValue(part));
                }
            }
        }
        catch (IOException | ServletException exception) {

            LoggerFactory.getLogger(AbstractRequestProcessor.class).error(null, exception);
            throw new RuntimeException(exception);
        }

        for (Part delete : toDelete) {

            try {
                delete.delete();
            }
            catch (IOException exception) {

                LoggerFactory.getLogger(AbstractRequestProcessor.class).error(null, exception);
            }
        }
    }

    /**
     * Returns whether the specified part is a file part.
     * 
     * @param part The part to test.
     * @return True if the part is a file part, otherwise a value of false is returned.
     */
    private boolean isFilePart(Part part) {

        String contentType = part.getContentType();
        if (contentType == null) {
            return false;
        }

        return (contentType.contains("java") ||
                contentType.contains("zip") ||
                contentType.contains("jar") ||
                contentType.equals("application/octet-stream"));
    }

    /**
     * Returns the value of the specified part.
     * 
     * @param part The part to parse.
     * @return The string value.
     * @throws IOException If the part cannot be parsed.
     */
    private String getValue(Part part) throws IOException {

        BufferedReader reader = null;

        try {

            reader = new BufferedReader(new InputStreamReader(part.getInputStream(), encoding));
            StringBuilder value = new StringBuilder();
            char[] buffer = new char[8192];

            int length = reader.read(buffer);

            while (length > 0) {

                value.append(buffer, 0, length);
                length = reader.read(buffer);
            }

            reader.close();
            return value.toString();
        }
        finally {

            if (reader != null) {

                reader.close();
            }
        }
    }

    /**
     * Processes a file part.
     * 
     * @param name The string file name.
     * @param inputStream The input stream.
     * @throws IOException If the file part cannot be parsed.
     */
    protected abstract void processFilePart(String name, InputStream inputStream) throws IOException;

    /**
     * Processes the part name and value.
     * 
     * @param name The string part name.
     * @param value The string part value.
     */
    protected abstract void processPart(String name, String value);
}
