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

import java.util.*;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.SignalIndication;

/**
 * A JAXB adapter to marshal and unmarshal a generic map of lists. For the Lane Detector and Signal
 * Indication classes
 * 
 * @author bbadillo
 * @param <T> Generic for the manager.
 */
@XmlSeeAlso(value = { Lane.class, Detector.class, SignalIndication.class })
public class JaxbMapOfListsAdapter<T> extends XmlAdapter<JaxbMapOfListsAdapter.ListMap<T>, Map<Integer, List<T>>> {

    /**
     * Marshals a generic from a map to a list
     * 
     * @param v The list to marshal
     * @return The map of the list marshaled
     */
    @Override
    public ListMap<T> marshal(Map<Integer, List<T>> v) {
        if (null == v) {
            return null;
        }
        ListMap<T> listMap = new ListMap<T>();
        for (Map.Entry<Integer, List<T>> entry : v.entrySet()) {
            listMap.listEntry.add(new ListEntry<T>(entry.getKey(), entry.getValue()));
        }
        return listMap;
    }

    /**
     * Unmarshals a generic list to a map
     * 
     * @param v the map to unmarshal
     * @return map The map
     */
    @Override
    public Map<Integer, List<T>> unmarshal(ListMap<T> v) {
        if (null == v) {
            return null;
        }
        Map<Integer, List<T>> map = new HashMap<Integer, List<T>>();
        for (ListEntry<T> entry : v.listEntry) {
            map.put(entry.key, entry.list);
        }
        return map;
    }

    /**
     * List for adapted elements.
     * 
     * @param <T> Generic for the manager.
     */
    public static class ListMap<T> {

        public List<ListEntry<T>> listEntry = new ArrayList<ListEntry<T>>();
    }

    /**
     * The converted entry class.
     * 
     * @author jrutherford
     * @param <T> Generic for the manager.
     */
    public static class ListEntry<T> {

        /** The key. */
        public Integer key;

        /** The list. */
        @XmlElement(name = "value")
        public List<T> list;

        /** Empty Constructor. */
        public ListEntry() {}

        /**
         * Constructor.
         * 
         * @param key The key.
         * @param list The list.
         */
        public ListEntry(Integer key, List<T> list) {
            this.key = key;
            this.list = list;
        }
    }
}