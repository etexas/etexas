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

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Defines a DSRC device that replays the DSRC messages from a source PDML file.
 * 
 * @author emyers
 */
public class DsrcUnit {

    /**
     * The name of the DSRC unit.
     */
    private String name;

    /**
     * The path to the source PDML file.
     */
    private Path sourcePath;

    /**
     * The DSRC message listeners.
     */
    private List<DsrcListener> listeners;

    /**
     * The flag to indicate the current playing status.
     */
    private boolean isPlaying;

    /**
     * The flag to indicate the current parsing status.
     */
    private boolean isParsing;

    /**
     * The current DSRC information packet.
     */
    private DsrcPacket packet;

    /**
     * The latitude of the unit (dd).
     */
    private double latitude;

    /**
     * The longitude of the unit (dd).
     */
    private double longitude;

    /**
     * Creates a new <code>DsrcUnit</code> player for the given source PDML file.
     * 
     * @param name the name of the DSRC unit
     * @param sourcePath the path to the source PDML file
     * @throws IllegalArgumentException if the source file is not a PDML file
     * @throws NullPointerException if a <code>null</code> value is given for the DSRC unit name or
     *         the source PDML file path
     */
    public DsrcUnit(String name, Path sourcePath) {

        // if no unit name exists
        if (name == null) {

            // throw a NullPointerException to indicate the error
            throw new NullPointerException("a unit name must be provided");
        }

        // if no source PDML file exists
        if (sourcePath == null) {

            // throw a NullPointerException to indicate the error
            throw new NullPointerException("a source PDML file must be provided");
        }

        // if the source file is not the proper format
        if (!sourcePath.toString().endsWith(".pdml")) {

            // throw an IllegalArgumentException to indicate the error
            throw new IllegalArgumentException(
                    "the source file not a valid .pdml file");
        }

        this.name = name;
        this.sourcePath = sourcePath;
        listeners = new ArrayList<DsrcListener>();
    }

    /**
     * Returns the name of the DSRC unit.
     * 
     * @return the name of the unit
     */
    public String getName() {

        return name;
    }

    /**
     * Sets the name of the DSRC unit.
     * 
     * @param name the name of the DSRC unit
     */
    public void setName(String name) {

        this.name = name;
    }

    /**
     * Returns the latitude (dd) of the DSRC unit.
     * 
     * @return the latitude (dd) of the DSRC unit
     */
    public double getLatitude() {

        return latitude;
    }

    /**
     * Sets the latitude (dd) of the DSRC unit.
     * 
     * @param latitude the latitude (dd) of the DSRC unit
     * @throws IllegalArgumentException if the latitude is not valid
     */
    public void setLatitude(double latitude) {

        // if the given latitude is undefined
        if (latitude == DsrcUtils.LAT_ERROR) {

            // throw an IllegalArgumentException to indicate the error
            throw new IllegalArgumentException("undefined latitude");
        }

        // if the given latitude is out of range
        if (latitude < -90.0 || latitude > 90.0) {

            // throw an IllegalArgumentException to indicate the error
            throw new IllegalArgumentException("latitude out of range");
        }

        this.latitude = latitude;
    }

    /**
     * Returns the longitude (dd) of the DSRC unit.
     * 
     * @return the longitude (dd) of the DSRC unit
     */
    public double getLongitude() {

        return longitude;
    }

    /**
     * Sets the longitude (dd) of the DSRC unit.
     * 
     * @param longitude the longitude (dd) of the DSRC unit
     * @throws IllegalArgumentException if the longitude is not valid
     */
    public void setLongitude(double longitude) {

        // if the given longitude is undefined
        if (longitude == DsrcUtils.LON_ERROR) {

            // throw an IllegalArgumentException to indicate the error
            throw new IllegalArgumentException("undefined longitude");
        }

        // if the given longitude is out of range
        if (longitude < -180 || longitude > 180) {

            // throw an IllegalArgumentException to indicate the error
            throw new IllegalArgumentException("longitude out of range");
        }

        this.longitude = longitude;
    }

    /**
     * Adds the DSRC listener to those registered for DSRC message notifications. DSRC listeners
     * cannot be added more than once.
     * 
     * @param listener the DSRC listener to register
     * @throws NullPointerException if a <code>null</code> value is given for the DSRC listener
     */
    public void addDsrcListener(DsrcListener listener) {

        // if no listener is given
        if (listener == null) {

            // throw a NullPointerException to indicate the error
            throw new NullPointerException("a valid listener must be provided");
        }

        // if the listener is not already registered
        if (!listeners.contains(listener)) {

            // add the listener for DSRc message notifications
            listeners.add(listener);
        }
    }

    /**
     * Removes the DSRC listener from those registered for DSRC message notifications.
     * 
     * @param listener the DSRC listener to remove
     */
    public void removeDsrcListener(DsrcListener listener) {

        listeners.remove(listener);
    }

    /**
     * Forwards the given DSRC message to all registered listeners.
     * 
     * @param message the DSRC message to forward
     */
    private void fireMessageEvent(DsrcMessage message) {

        // forward the message to all registered listeners
        for (DsrcListener listener : listeners) {
            listener.messageReceived(message);
        }
    }

    /**
     * Fires the next DSRC message event in the source PDML file. If the end of the file has been
     * reached, messages will resume from the beginning of the source PDML file. To prevent
     * repetition, check the playing status with <code>isPlaying()</code> prior to invoking this
     * method.
     */
    public void play() {

        // if playing has stopped
        if (!isPlaying) {

            // resume playing from the beginning of the file
            new Thread(new PdmlParser()).start();
            while (!isPlaying) {}
        }
        // if playing is in progress
        else {

            // play the next DSRC message
            isParsing = true;
            while (isParsing) {}
        }
    }

    /**
     * Returns the playing status of the unit.
     * 
     * @return <code>true</code> if more packets exist in the source PDML file, otherwise a value of
     *         <code>false</code> is returned
     */
    public boolean isPlaying() {

        return isPlaying;
    }

    /**
     * Defines methods to parse the source PDML file.
     */
    private class PdmlParser extends DefaultHandler implements Runnable {

        @Override
        public void run() {

            // initialize the factory for parser object creation
            SAXParserFactory parserFactory = SAXParserFactory.newInstance();
            parserFactory.setNamespaceAware(true);

            try {

                // initialize source PDML file parsing
                XMLReader reader = parserFactory.newSAXParser().getXMLReader();
                reader.setContentHandler(this);
                reader.parse(sourcePath.toString());
            }
            catch (Exception exception) {

                // display the exception
                exception.printStackTrace();
            }
        }

        @Override
        public void startElement(String uri, String localName, String qName, Attributes attributes) {

            try {

                // if a new packet element is reached
                if (isPacketElement(qName)) {

                    // create a new DSRC information packet
                    packet = new DsrcPacket();
                }
                // if the epoch time element is reached
                else if (isEpochTimeElement(qName, attributes)) {

                    // set the epoch time value
                    packet.setTime(attributes.getValue("show"));
                }
                // if the transmission element is reached
                else if (isTxElement(qName, attributes)) {

                    // set the transmission value
                    packet.setTx(attributes.getValue("value"));
                }
                // if the PSID element is reached
                else if (isPsidElement(qName, attributes)) {

                    // set the PSID value
                    packet.setPsid(attributes.getValue("value"));
                }
                // if the WSM element is reached
                else if (isWsmElement(qName, attributes)) {

                    // set the WSM value
                    packet.setWsm(attributes.getValue("value"));
                }
            }
            catch (Exception exception) {

                // flag that the packet was malformed
                packet.setMalformed(true);
            }
        }

        @Override
        public void endElement(String uri, String localName, String qName) {

            // if a packet element has ended
            if (isPacketElement(qName)) {

                // fire the parsed DSRC message
                fireMessageEvent(new DsrcMessage(DsrcUnit.this, packet));

                // pause until playing is resumed
                isParsing = false;
                while (!isParsing) {}
            }
        }

        @Override
        public void startDocument() {

            // flag that playing has started
            isPlaying = true;
        }

        @Override
        public void endDocument() {

            // flag that playing is complete
            isPlaying = false;
            isParsing = false;
        }

        /**
         * Returns whether the current element is a field element.
         * 
         * @param name the element name
         * @return <code>true</code> if the current element is a field element, otherwise a value of
         *         <code>false</code> is returned
         */
        private boolean isFieldElement(String name) {
            return name.equals("field");
        }

        /**
         * Returns whether the current element is a packet element.
         * 
         * @param name the element name
         * @return <code>true</code> if the current element is a packet element, otherwise a value
         *         of <code>false</code> is returned
         */
        private boolean isPacketElement(String name) {
            return name.equals("packet");
        }

        /**
         * Returns whether the current element is the epoch time element.
         * 
         * @param name the element name
         * @param attributes the element attributes
         * @return <code>true</code> if the current element is the epoch time element, otherwise a
         *         value of <code>false</code> is returned
         */
        private boolean isEpochTimeElement(String name, Attributes attributes) {

            return isFieldElement(name) && attributes.getValue("name").equals("frame.time_epoch");
        }

        /**
         * Returns whether the current element is the transmission element.
         * 
         * @param name the element name
         * @param attributes the element attributes
         * @return <code>true</code> if the current element is the transmission element, otherwise a
         *         value of <code>false</code> is returned
         */
        private boolean isTxElement(String name, Attributes attributes) {

            return isFieldElement(name) && attributes.getValue("name").equals("prism.did.istx");
        }

        /**
         * Returns whether the current element is the PSID element.
         * 
         * @param name the element name
         * @param attributes the element attributes
         * @return <code>true</code> if the current element is the PSID element, otherwise a value
         *         of <code>false</code> is returned
         */
        private boolean isPsidElement(String name, Attributes attributes) {

            return isFieldElement(name) && attributes.getValue("name").equals("wsmp.psid");
        }

        /**
         * Returns whether the current element is the WSM element.
         * 
         * @param name the element name
         * @param attributes the element attributes
         * @return <code>true</code> if the current element is the WSM element, otherwise a value of
         *         <code>false</code> is returned
         */
        private boolean isWsmElement(String name, Attributes attributes) {

            return isFieldElement(name) && attributes.getValue("show").equals("Wave Short Message");
        }
    }
}