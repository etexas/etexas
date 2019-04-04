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
package org.etexascode.appslayerdata;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;

/**
 * A snapshot of RSE device information for a particular time step.
 * 
 * @author ablatt
 * @author emyers
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class RSEDeviceInfo extends AbstractDeviceInfo {

    /** The serial version ID. */
    private static final long serialVersionUID = -4006504578540120216L;

    /** The map of signal managers for the supported intersections. */
    @XmlAnyElement(lax = true)
    final Map<Integer, ISignalManager> signalMap;

    /** The map of lane managers for the supported intersections. */
    @XmlAnyElement(lax = true)
    final Map<Integer, ILaneManager> laneMap;

    /** The map of detector managers for the support intersections. */
    @XmlAnyElement(lax = true)
    final Map<Integer, IDetectorManager> detectorMap;

    /** The reference points for the RSE device. */
    @XmlElement
    private final ReferencePoint[] referencePoints;

    /** The proper ID for the RSE device. */
    private String properId = null;

    /** The location of the RSE device. */
    @XmlAnyElement(lax = true)
    private final IDistanceable location;

    // needed for JAXB
    public RSEDeviceInfo() {

        this.signalMap = null;
        this.laneMap = null;
        this.detectorMap = null;
        this.referencePoints = null;
        this.location = null;
    }

    /**
     * Constructs a new <code>RSEDeviceInfo</code> with the specified information.
     * 
     * @param signalMap The map of signal managers for supported intersections.
     * @param laneMap The map of lane managers for supported intersections.
     * @param detectorMap The map of detectors for supported intersections.
     * @param messages The list of received messages.
     * @param referencePoints The reference points.
     * @param deviceId The MAC address of the device.
     * @param location The location of the device.
     */
    public RSEDeviceInfo(Map<Integer, ISignalManager> signalMap, Map<Integer, ILaneManager> laneMap, Map<Integer, IDetectorManager> detectorMap, List<BasicMessage> messages,
            ReferencePoint[] referencePoints, long deviceId, IDistanceable location) {

        super(messages, deviceId);

        this.signalMap = new HashMap<Integer, ISignalManager>(signalMap);
        this.laneMap = new HashMap<Integer, ILaneManager>(laneMap);
        this.detectorMap = new HashMap<Integer, IDetectorManager>(detectorMap);
        this.referencePoints = Arrays.copyOf(referencePoints, referencePoints.length);
        this.location = location;
    }

    /**
     * Returns the reference points for the RSE device.
     * 
     * @return The reference points for the RSE device.
     */
    public ReferencePoint[] getReferencePoints() {

        return (referencePoints == null) ? null : referencePoints.clone();
    }

    @Override
    public boolean equalsId(IDable entity) {

        return getProperId().equals(entity.getProperId());
    }

    @Override
    public String getProperId() {

        if (properId == null) {

            properId = String.format("RSEDevice:%d", getDeviceId());
        }

        return properId;
    }

    @Override
    public IDistanceable getLocation() {

        return location;
    }

    /**
     * Returns the signal manager for the specified intersection.
     * 
     * @param intersection The ID of the intersection.
     * @return The signal manager for the specified intersection.
     */
    public ISignalManager getSignalManager(Integer intersection) {

        return signalMap.get(intersection);
    }

    /**
     * Returns the signal managers for the supported intersections.
     * 
     * @return A map of signal managers for the supported intersections.
     */
    public Map<Integer, ISignalManager> getSignalManagers() {

        return signalMap;
    }

    /**
     * Returns the lane manager for the specified intersection.
     * 
     * @param intersection The ID of the intersection.
     * @return The lane manager for the specified intersection.
     */
    public ILaneManager getLaneManager(Integer intersection) {

        return laneMap.get(intersection);
    }

    /**
     * Returns the lane managers for the supported intersections.
     * 
     * @return A map of lane managers for the supported intersections.
     */
    public Map<Integer, ILaneManager> getLaneManagers() {

        return laneMap;
    }

    /**
     * Returns the detector manager for the specified intersection.
     * 
     * @param intersection The ID of the intersection.
     * @return The detector manager for the specified intersection.
     */
    public IDetectorManager getDetectorManager(Integer intersection) {

        return detectorMap.get(intersection);
    }

    /**
     * Returns the detector managers for the supported intersections.
     * 
     * @return A map of detector managers for the supported intersections.
     */
    public Map<Integer, IDetectorManager> getDetectorManagers() {

        return detectorMap;
    }
}
