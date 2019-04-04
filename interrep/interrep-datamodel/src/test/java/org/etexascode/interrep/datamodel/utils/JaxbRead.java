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

package org.etexascode.interrep.datamodel.utils;

import java.io.File;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Generic JAXB reader- reads generic Manager from xml file
 * 
 * @author bmauldon
 */
public class JaxbRead {

    /** Static logger */
    private static final Logger LOGGER = LoggerFactory.getLogger(JaxbRead.class);

    /**
     * Reads an object of the given class from the xml file with the given name on this class's
     * classpath.
     * 
     * @param xmlfile The name of the file to read from (on this class's classpath).
     * @param clazz The class being read in.
     * @return manager The object represented by the xml file.
     */
    @SuppressWarnings("unchecked")
    public static <T> T readJaxManager(String xmlfile, Class clazz) {

        T manager = null;

        try {
            JAXBContext jc = JAXBContext.newInstance(clazz);
            Unmarshaller u = jc.createUnmarshaller();
            File resource = FileUtils.toFile(JaxbRead.class.getClass().getResource(xmlfile));

            if (resource == null) {
                LOGGER.error("File was null during unmarshalling.");
            }

            manager = (T)u.unmarshal(resource);

        }
        catch (JAXBException ex) {
            LOGGER.debug("JAXB Exception", ex);
            ex.printStackTrace();
        }
        return manager;

    }

    /**
     * Reads an object of the given class from the given xml File.
     * 
     * @param xmlfile The file to read from.
     * @param clazz The class being read in.
     * @return The object represented by the xml file.
     */
    public static <T> T readJaxManager(File xmlfile, Class clazz) {
        T manager = null;

        try {
            JAXBContext jc = JAXBContext.newInstance(clazz);
            Unmarshaller u = jc.createUnmarshaller();
            manager = (T)u.unmarshal(xmlfile);
        }
        catch (JAXBException ex) {
            LOGGER.debug("JAXB Exception", ex);
        }
        return manager;
    }

}
