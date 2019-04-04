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

import java.util.Collection;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;

/**
 * A snapshot of report device information at a particular time step.
 *
 * @author ablatt
 * @author emyers
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class ReportDeviceInfo extends AbstractDeviceInfo {

    /** The serial version ID. */
    private static final long serialVersionUID = -3390124045367601999L;

    /** The map of vehicle managers for the supported intersections. */
    @XmlAnyElement(lax = true)
    final Map<Integer, IVehicleManager> vehicleMap;

    /** The map of signal managers for the supported intersections. */
    @XmlAnyElement(lax = true)
    final Map<Integer, ISignalManager> signalMap;

    /** The map of lane managers for the supported intersections. */
    @XmlAnyElement(lax = true)
    final Map<Integer, ILaneManager> laneMap;

    /** The map of detector managers for the supported intersections. */
    @XmlAnyElement(lax = true)
    final Map<Integer, IDetectorManager> detectorMap;

    /** The transmitted messages. */
    @XmlElement
    private final Collection<BasicMessage> txMessages;

    /** The proper ID for the report device. */
    private String properId = null;

    /** The location of the report device. */
    @XmlAnyElement(lax = true)
    private final IDistanceable location;

    // needed for JAXB
    public ReportDeviceInfo() {

        this.vehicleMap = null;
        this.signalMap = null;
        this.laneMap = null;
        this.detectorMap = null;
        this.location = null;
        this.txMessages = null;
    }

    /**
     * Constructs a new <code>ReportDeviceInfo</code> with the specified information.
     *
     * @param vehicleMap The map of vehicle managers for supported intersections.
     * @param signalMap The map of signal managers for supported intersections.
     * @param laneMap The map of lane managers for supported intersections.
     * @param detectorMap The map of detector managers for supported intersections.
     * @param txMessages The collection of transmitted messages.
     * @param rxMessages The collection of received messages.
     * @param devId The MAC address of the report device.
     * @param point The location of the report device.
     */
    public ReportDeviceInfo(Map<Integer, IVehicleManager> vehicleMap, Map<Integer, ISignalManager> signalMap, Map<Integer, ILaneManager> laneMap, Map<Integer, IDetectorManager> detectorMap,
            Collection<BasicMessage> txMessages, Collection<BasicMessage> rxMessages, long devId, IDistanceable point) {

        super(rxMessages, devId);
        this.vehicleMap = vehicleMap;
        this.signalMap = signalMap;
        this.laneMap = laneMap;
        this.detectorMap = detectorMap;
        this.location = point;
        this.txMessages = txMessages;
    }

    /**
     * Equality check, mainly for testing
     */
    @Override
    public boolean equalsId(IDable entity) {
        return getProperId().equals(entity.getProperId());
    }

    /**
     * Gets the proper id of the report device.
     * 
     * @return properId The proper id for the report device
     */
    @Override
    public String getProperId() {

        if (properId == null) {

            properId = String.format("ReportDevice:%d", deviceId);
        }

        return properId;
    }

    /**
     * Gets the location of the report device.
     * 
     * @return location The location of the report device
     */
    @Override
    public IDistanceable getLocation() {
        return location;
    }

    /**
     * Returns the vehicle manager for the specified intersection.
     * 
     * @param intersection The ID of the intersection.
     * @return The vehicle manager for the specified intersection.
     */
    public IVehicleManager getVehicleManager(int intersection) {

        return vehicleMap.get(intersection);
    }

    /**
     * Returns the vehicle managers for the supported intersections.
     * 
     * @return A map of vehicle managers for the supported intersections.
     */
    public Map<Integer, IVehicleManager> getVehicleManagers() {

        return vehicleMap;
    }

    /**
     * Returns the signal manager for the specified intersection.
     * 
     * @param intersection The ID of the intersection.
     * @return The signal manager for the specified intersection.
     */
    public ISignalManager getSignalManager(int intersection) {

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
    public ILaneManager getLaneManager(int intersection) {

        return laneMap.get(intersection);
    }

    /**
     * Returns the lane manager for the supported intersections.
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
    public IDetectorManager getDetectorManager(int intersection) {

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

    /**
     * Returns the transmitted messages.
     * 
     * @return A collection of transmitted messages.
     */
    public Collection<BasicMessage> getTxMessages() {

        return txMessages;
    }
}
