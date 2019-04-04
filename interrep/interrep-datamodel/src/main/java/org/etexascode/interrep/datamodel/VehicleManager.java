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
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.interrep.datamodel.interfaces.Shiftable;
import org.etexascode.interrep.datamodel.utils.UtilsStringOnModel;
import org.etexascode.interrep.datamodel.xmladapters.ManagerAdapterString;

/**
 * Manages all of the vehicles in the intersection
 *
 * @author egaebel
 * @author dranker
 * @author bbadillo
 * @author ablatt
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlSeeAlso(value = Vehicle.class)
@XmlRootElement(name = "VehicleManager")
public class VehicleManager implements Serializable, IVehicleManager, Shiftable {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = 1L;

    /**
     * The number of history items that should be kept for this vehicle
     */
    @XmlElement
    private static final int VEHICLE_HISTORY_SIZE = 4;

    /**
     * A number representing the tolerance to error in computing equal double values.
     */
    @XmlElement
    private static final double ERROR_TOLERANCE = 0.01;

    /**
     * Most recent message's time
     */
    @XmlElement
    private double currentMessageTime;

    /**
     * The current point that the ProjectedVehicleIds are at.
     */
    @XmlElement
    private static int currentProjectedVehicleID = 0;

    /**
     * The amount of time between messages at which point the vehicle will be removed from the
     * intersection. Set by default to 30, but can be changed by the user.
     */
    @XmlElement
    private int timeoutLength = 30;

    @XmlElement
    @XmlJavaTypeAdapter(ManagerAdapterString.class)
    private Map<String, Vehicle> vehicles = new HashMap<String, Vehicle>();

    // ~Constructors------------------------------------------------------------------------
    /**
     * Default constructor, empty.
     */
    public VehicleManager() {}

    public VehicleManager(VehicleManager vehicleManager) {
        this.currentMessageTime = vehicleManager.currentMessageTime;
        this.timeoutLength = vehicleManager.timeoutLength;
        this.vehicles = new HashMap<String, Vehicle>();
        for (Entry<String, Vehicle> entry : vehicleManager.vehicles.entrySet()) {

            this.vehicles.put(entry.getKey(), new Vehicle(entry.getValue()));
        }
    }

    /**
     * Creates a string representation of the vehicle manager
     * 
     * @return The string representation
     */
    @Override
    public String toString() {
        StringBuilder ret = new StringBuilder();

        // UtilsStringOnModel.addString(ret,
        // UtilsLatLongConvertion.convertCalculatorType(geoCalculatorType), "calculator type");
        UtilsStringOnModel.addMap(ret, vehicles, "vehicles");

        return ret.toString();
    }

    /**
     * Add a list of vehicles to this manager
     * 
     * @param vehicles A list of vehicles to add to this manager
     */
    public void addVehicles(List<Vehicle> vehicles) {
        this.vehicles = new HashMap<String, Vehicle>();

        for (Vehicle v : vehicles) {
            this.vehicles.put(v.getProperId(), v);
        }
    }

    /**
     * Get a specific vehicle from the manager
     * 
     * @param vehicleId The id of the vehicle you want to look at
     * @return The vehicle with the specified id
     */
    @Override
    public Vehicle getVehicle(String vehicleId) {
        return vehicles.get(vehicleId);
    }

    /**
     * Takes in a vehicleId and removes the vehicle denoted by that id from the vehicles map. TODO:
     * ablatt - remove this method
     * 
     * @param vehicleId the id of the vehicle to remove.
     */
    public void removeVehicle(String vehicleId) {
        vehicles.remove(vehicleId);
    }

    /**
     * Get a list of vehicles in a specified lane.
     * 
     * @param laneId The id of the lane to check for vehicles.
     * @return A list of vehicles in the specified lane ordered from closest to intersection to
     *         farthest.
     */
    @Override
    public List<Vehicle> getVehiclesInLane(int laneId) {
        List<Vehicle> retList = new LinkedList<Vehicle>();

        for (Vehicle v : vehicles.values()) {
            if (v.getLaneID() == laneId) {
                retList.add(0, v);
            }
        }

        return retList;
    }

    /**
     * Checks if 2 vehicle managers are equal
     * 
     * @param obj The vehicle manager to compare to
     * @return True/False the vehicle managers are the same
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof VehicleManager) {
            VehicleManager vm = (VehicleManager)obj;
            Set<Entry<String, Vehicle>> thisEntries = vehicles.entrySet();
            Set<Entry<String, Vehicle>> vmEntries = vehicles.entrySet();

            if (thisEntries.size() != vmEntries.size()) {
                return false;
            }

            for (Entry<String, Vehicle> entry : thisEntries) {
                if (!entry.getValue().equals(vm.vehicles.get(entry.getKey()))) {
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
     * HashCode operation. TODO: ablatt - this operation is wrong...
     * 
     * @return A hashcode of the data.
     */
    @CoberturaIgnore
    @Override
    public int hashCode() {
        return new HashCodeBuilder(77, 19).toHashCode();
    }

    /**
     * Get all the vehicle ids in this vehicle manager
     * 
     * @return The ids of all the vehicles in the vehicle manager
     */
    @Override
    public Set<String> getAllVehicleIds() {
        return vehicles.keySet();
    }

    /**
     * Add a vehicle to this manager
     * 
     * @param v The vehicle to add to the manager
     */
    public void addVehicle(Vehicle v) {
        vehicles.put(v.getProperId(), v);
    }

    /**
     * Special iterator helper to parse through vehicles in the manager
     * 
     * @return The iterator helper
     */
    @CoberturaIgnore
    @Override
    public Iterator<IVehicle> iterator() {
        return new IteratorHelper<IVehicle, Vehicle>(vehicles.values().iterator());
    }

    /**
     * Special iterator to parse vehicles
     * 
     * @return The iterator
     */
    public Iterable<Vehicle> getIterable() {
        return vehicles.values();
    }

    @Override
    public void shift(double deltaX, double deltaY) {

        for (Vehicle v : this.vehicles.values()) {

            v.setX(v.getX() + deltaX);
            v.setY(v.getY() + deltaY);
        }
    }
}