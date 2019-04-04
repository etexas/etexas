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

package org.etexascode.interrep.datamodel;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.CoberturaIgnore;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.xmladapters.ManagerAdapter;

/**
 * The detector manager model.
 * 
 * @author ablatt
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
@XmlSeeAlso(Detector.class)
public class DetectorManager implements Serializable, IDetectorManager {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = 7762341650692544779L;

    /** The map of detectors, keyed by detector id. */
    @XmlElement
    @XmlJavaTypeAdapter(ManagerAdapter.class)
    private Map<Integer, Detector> detectors;

    /** Constructor. */
    public DetectorManager() {
        detectors = new HashMap<Integer, Detector>();
    }

    /**
     * Adds a detector to the list.
     * 
     * @param key The key.
     * @param value The detector.
     */
    public void addDetector(Integer key, Detector value) {
        detectors.put(key, value);
    }

    /** Manager as a string representation. */
    public void clearDetectors() {
        detectors = new HashMap<Integer, Detector>();
    }

    /** Checks if 2 detector managers are equal. */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof DetectorManager) {
            DetectorManager dm = (DetectorManager)obj;

            Set<Integer> thisKeys = detectors.keySet();
            Set<Integer> dmKeys = dm.getKeys();

            if (thisKeys.size() != dmKeys.size() || !(thisKeys.containsAll(dmKeys) && dmKeys.containsAll(thisKeys))) {
                return false;
            }

            for (Integer i : dmKeys) {
                if (!detectors.get(i).equals(dm.detectors.get(i))) {
                    return false;
                }
            }

            return true;
        }
        else {
            return false;
        }

    }

    /**
     * HashCode operation.
     * 
     * @return A hashcode of the data.
     */
    @Override
    @CoberturaIgnore
    public int hashCode() {
        return new HashCodeBuilder(23, 11).toHashCode();
    }

    /**
     * Gets the detector with the given ID..
     * 
     * @param id The ID.
     * @return The detector.
     */
    @Override
    public Detector getDetector(Integer id) {
        return detectors.get(id);
    }

    /**
     * Gets all the detectors.
     * 
     * @return The collection of detectors.
     */
    @Override
    public Collection<Detector> getDetectorCollection() {
        return detectors.values();
    }

    /**
     * Gets the keys.
     * 
     * @return The list of keys.
     */
    @Override
    public Set<Integer> getKeys() {
        return detectors.keySet();
    }

    /**
     * Create a string out of this detector manager.
     */
    @Override
    public String toString() {
        StringBuilder ret = new StringBuilder();

        ret.append("Detectors:\n");

        for (Entry<Integer, Detector> entry : detectors.entrySet()) {
            ret.append(entry.getKey().toString());
            ret.append(":");
            ret.append(entry.getValue().toString());
        }

        ret.append("=====\n\n");

        return ret.toString();
    }

    /**
     * Gets the detectors in this detector manager
     * 
     * @return The detectors
     */
    public Iterable<Detector> getIterable() {
        return detectors.values();
    }

    /**
     * Iterates through the detectors in this detector manager
     */
    @CoberturaIgnore
    @Override
    public Iterator<IDetector> iterator() {
        return new IteratorHelper<IDetector, Detector>(detectors.values().iterator());
    }

    /**
     * Gets detectors to be added to the detector manager from the lanes
     * 
     * @param id the lane id to get detector information from
     * @return ret The detectors added to the manager from the lane
     */
    @Override
    public List<? extends IDetector> getDetectorFromLaneId(int id) {
        List<Detector> ret = new LinkedList<Detector>();
        for (Detector d : detectors.values()) {
            if (d.getLaneIDs().contains(id)) {
                ret.add(d);
            }
        }
        return ret;
    }
}