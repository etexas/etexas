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

package org.etexascode.interrep.datamodel.xmladapters;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.etexascode.interrep.datamodel.Vehicle;

/**
 * The adapter for the map of detectors. This only works for Strings For use with the VehicleManager
 * Class
 * 
 * @author ablatt
 * @author jconnelly
 */
@XmlSeeAlso(value = { Vehicle.class })
public class ManagerAdapterString<T> extends
        XmlAdapter<ManagerAdapterString.XmlMap<T>, Map<String, T>> {

    /**
     * Marshals a manager adapter to xml
     * 
     * @param v the map to marshal
     * @return map The map
     */
    @Override
    public ManagerAdapterString.XmlMap<T> marshal(Map<String, T> v)
            throws Exception {
        if (null == v) {
            return null;
        }
        ManagerAdapterString.XmlMap<T> map = new ManagerAdapterString.XmlMap<T>();
        for (Map.Entry<String, T> entry : v.entrySet()) {
            map.entry.add(new Entry<T>(entry.getKey(), entry.getValue()));
        }
        return map;
    }

    /**
     * Unmarshals a manager adapter from xml
     * 
     * @param v The xml map
     * @return The manager adapter map
     */
    @Override
    public Map<String, T> unmarshal(XmlMap<T> v) throws Exception {
        if (null == v) {
            return null;
        }
        Map<String, T> map = new HashMap<String, T>();
        for (Entry<T> entry : v.entry) {
            map.put(entry.key, entry.value);
        }
        return map;
    }

    /**
     * List for adapted map.
     */
    @XmlType(name = "XmlMapString")
    public static class XmlMap<T> {

        public List<Entry<T>> entry = new ArrayList<Entry<T>>();
    }

    /**
     * The converted entry class.
     * 
     * @author jrutherford
     */
    @XmlType(name = "EntryString")
    public static class Entry<T> {

        /**
         * The key.
         */
        public String key;

        /**
         * The detector.
         */
        @XmlElement
        public T value;

        /**
         * Empty Constructor.
         */
        public Entry() {}

        /**
         * Constructor.
         * 
         * @param key The key of the entry.
         * @param value The value of the entry.
         */
        public Entry(String key, T value) {
            this.key = key;
            this.value = value;
        }
    }
}
