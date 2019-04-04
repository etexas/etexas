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
import java.util.List;
import java.util.Map;

import org.etexascode.devicedata.DualIntIdentifier;
import org.etexascode.wavesim.PropagationLossModel;

/**
 * Messaging class for passing metadata into an execution.
 * 
 * @author ablatt
 * @author ttevendale
 */
public class ExecMetaData implements Serializable {

    /** Serial ID. */
    private static final long serialVersionUID = 5981384824946380108L;

    /** The ID of the simulation being executed */
    long simulationId;

    /** The reference points to be used by the execution. */
    ReferencePoint[] referencePoints;

    /** The calculator type to be used during the current execution. */
    int geoCalculatorType = 1;

    /** The propagation loss model to be used during the current execution. */
    PropagationLossModel propagationLossModel;

    /** Simulated detectors for this specific execution */
    List<Detector> execDetectors;

    /** The x coordinate of the model */
    double x;

    /** The y coordinate of the model */
    double y;

    /** The lane map for the model. */
    private transient Map<DualIntIdentifier, DualIntIdentifier> laneMap;

    /**
     * Gets the ID of the simulation being executed.
     * 
     * @return The simulation ID.
     */
    public long getSimulationId() {

        return simulationId;
    }

    /**
     * Sets the ID of the simulation being executed.
     * 
     * @param simulationId The simulation ID.
     */
    public void setSimulationId(long simulationId) {

        this.simulationId = simulationId;
    }

    /**
     * Returns the actual array of reference points.
     * 
     * @return Actual array of reference points.
     */
    public ReferencePoint[] getReferencePoints() {
        if (referencePoints == null) {
            return null;
        }
        else {
            return referencePoints.clone();
        }
    }

    /**
     * Set the actual array of reference points into the message.
     * 
     * @param referencePoints The reference points.
     */
    public void setReferencePoints(ReferencePoint[] referencePoints) {
        if (referencePoints == null) {
            this.referencePoints = null;
        }
        else {
            this.referencePoints = referencePoints.clone();
        }
    }

    /**
     * Getter for the calculator type.
     * 
     * @return The calculator type.
     */
    public int getGeoCalculatorType() {
        return geoCalculatorType;
    }

    /**
     * Setter for the calculator type.
     * 
     * @param geoCalculatorType The geographic calculator for this execution.
     */
    public void setGeoCalculatorType(int geoCalculatorType) {
        this.geoCalculatorType = geoCalculatorType;
    }

    /**
     * Getter for the propagation loss model.
     * 
     * @return The propagation loss model.
     */
    public PropagationLossModel getPropagationLossModel() {

        return propagationLossModel;
    }

    /**
     * Setter for the propagation loss model.
     * 
     * @param propagationLossModel The propagation loss model for this execution.
     */
    public void setPropagationLossModel(PropagationLossModel propagationLossModel) {

        this.propagationLossModel = propagationLossModel;
    }

    /**
     * Getter for the simulated detectors for the current execution.
     * 
     * @return The exec detectors.
     */
    public List<Detector> getExecDetectors() {
        return execDetectors;
    }

    /**
     * Setter for the simulated detectors for the current execution.
     * 
     * @param execDetectors The detectors for the current execution.
     */
    public void setExecDetectors(List<Detector> execDetectors) {
        this.execDetectors = execDetectors;
    }

    /**
     * Getter for the x coordinate of the model.
     * 
     * @return The x coordinate of the model.
     */
    public double getX() {
        return x;
    }

    /**
     * Setter for the x coordinate of the model.
     * 
     * @param x The x coordinate of the model.
     */
    public void setX(double x) {
        this.x = x;
    }

    /**
     * Getter for the y coordinate of the model.
     * 
     * @return The y coordinate of the model.
     */
    public double getY() {
        return y;
    }

    /**
     * Setter for the y coordinate of the model.
     * 
     * @param y The y coordinate of the model.
     */
    public void setY(double y) {
        this.y = y;
    }

    /**
     * Returns the lane map for the model.
     * 
     * @return The lane map for the model.
     */
    public Map<DualIntIdentifier, DualIntIdentifier> getLaneMap() {

        return laneMap;
    }

    /**
     * Sets the lane map for the model.
     * 
     * @param laneMap The lane map to set.
     */
    public void setLaneMap(Map<DualIntIdentifier, DualIntIdentifier> laneMap) {

        this.laneMap = laneMap;
    }
}
