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

package org.etexascode.apps;

import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;

/**
 * A structure to contain the latest known information about a queue.
 * 
 * @author bbadillo
 */
class QueueInfo {

    /**
     * The lane this queue is in.
     */
    private int laneId;

    /**
     * The last known stopped vehicle in the queue.
     */
    private IVehicle joining;

    /**
     * The first projected joining vehicle of the queue.
     */
    private IVehicle approaching;

    /**
     * The known leading vehicle in the queue.
     */
    private IVehicle leading;

    /**
     * The known departing vehicle of the queue.
     */
    private IVehicle departing;

    /**
     * The time when the first known vehicle in a queue starts to depart the queue.
     */
    private double departTime;

    /**
     * The latest time when the first joining vehicle was sensed.
     */
    private double approachTime;

    /**
     * The time when the last known vehicle stopped to form the end of the queue.
     */
    private double joinTime;

    /**
     * The time when the first known vehicle in a queue is detected as the first vehicle.
     */
    private double leadTime;

    /**
     * The projected queue length.
     */
    private double projectedQueue = 0.0;

    /**
     * The projected discharge position.
     */
    private double projectedDischarge = 0.0;

    /**
     * The rate at which this queue will grow (in centimeters per second).
     */
    private double maxGrowthRate;

    /**
     * The cumulative totals of queue lengths per cycle.
     */
    private double totalTravelTime;

    /**
     * The mode of accumulation for this queue.
     */
    private String mode;

    /**
     * Get the time of mode change for this queue.
     */
    private double changeTime;

    /**
     * A constant indicating that the queue can accumulate.
     */
    final public static String FULL_RED = "FULL_RED";

    /**
     * A constant indicating that the queue cannot accumulate.
     */
    final public static String FLOW = "FLOW";

    /**
     * Constructor
     * 
     * @param laneId The id of the lane represented by this object.
     */
    public QueueInfo(int laneId) {
        this.laneId = laneId;
        this.maxGrowthRate = UtilsUnitConversion.convertFeetPerSecondToCentimetersPerSecond(5.0); // cm
                                                                                                  // /
                                                                                                  // second
    }

    /**
     * Getter for the lane this queue is in.
     * 
     * @return The lane this queue is in.
     */
    public int getLaneId() {
        return laneId;
    }

    /**
     * Getter for the last known stopped vehicle in the queue.
     * 
     * @return The last known stopped vehicle in the queue.
     */
    public IVehicle getJoining() {
        return joining;
    }

    /**
     * Setter for the last known stopped vehicle in the queue.
     * 
     * @param joining The last known stopped vehicle in the queue.
     */
    public void setJoining(IVehicle joining) {
        this.joining = joining;
    }

    /**
     * Getter for the first projected joining vehicle of the queue.
     * 
     * @return The first projected joining vehicle of the queue.
     */
    public IVehicle getApproaching() {
        return approaching;
    }

    /**
     * Setter for the first projected joining vehicle of the queue.
     * 
     * @param approaching The first projected joining vehicle of the queue.
     */
    public void setApproaching(IVehicle approaching) {
        this.approaching = approaching;
    }

    /**
     * Getter for the latest time when the first joining vehicle was sensed.
     * 
     * @return The latest time when the first joining vehicle was sensed.
     */
    public double getApproachTime() {
        return approachTime;
    }

    /**
     * Setter for the latest time when the first joining vehicle was sensed.
     * 
     * @param approachTime The latest time when the first joining vehicle was sensed.
     */
    public void setApproachTime(double approachTime) {
        this.approachTime = approachTime;
    }

    /**
     * Getter for the time when the last known vehicle stopped to form the end of the queue.
     * 
     * @return The time when the last known vehicle stopped to form the end of the queue.
     */
    public double getJoinTime() {
        return joinTime;
    }

    /**
     * Setter for the time when the last known vehicle stopped to form the end of the queue.
     * 
     * @param joinTime The time when the last known vehicle stopped to form the end of the queue.
     */
    public void setJoinTime(double joinTime) {
        this.joinTime = joinTime;
    }

    /**
     * Getter for the known leading vehicle in the queue.
     * 
     * @return The known leading vehicle in the queue.
     */
    public IVehicle getLeading() {
        return leading;
    }

    /**
     * Setter for the known leading vehicle in the queue.
     * 
     * @param leading The known leading vehicle in the queue.
     */
    public void setLeading(IVehicle leading) {
        this.leading = leading;
    }

    /**
     * Getter for the time when the first known vehicle in the queue is sensed.
     * 
     * @return The time when the first known vehicle in the queue is sensed.
     */
    public double getLeadTime() {
        return leadTime;
    }

    /**
     * Setter for the time when the first known vehicle in the queue is sensed.
     * 
     * @param leadTime The time when the first known vehicle in the queue is sensed.
     */
    public void setLeadTime(double leadTime) {
        this.leadTime = leadTime;
    }

    /**
     * Getter for the known vehicle departing from the queue.
     * 
     * @return The known vehicle departing from the queue.
     */
    public IVehicle getDeparting() {
        return departing;
    }

    /**
     * Setter for the known vehicle departing from the queue.
     * 
     * @param departing The known vehicle departing from the queue.
     */
    public void setDeparting(IVehicle departing) {
        this.departing = departing;
    }

    /**
     * Get the time when the first known vehicle in a queue starts to depart the queue.
     * 
     * @return The time when the first known vehicle in a queue starts to depart the queue.
     */
    public double getDepartTime() {
        return departTime;
    }

    /**
     * Set the time when the first known vehicle in a queue starts to depart the queue.
     * 
     * @param departTime The time when the first known vehicle in a queue starts to depart the
     *        queue.
     */
    public void setDepartTime(double departTime) {
        this.departTime = departTime;
    }

    /**
     * Getter for the mode of accumulation for this queue.
     * 
     * @return The mode of accumulation for this queue.
     */
    public String getMode() {
        return mode;
    }

    /**
     * Setter for the mode of accumulation for this queue.
     * 
     * @param mode The mode of accumulation for this queue.
     */
    public void setMode(String mode) {
        this.mode = mode;
    }

    /**
     * Get the time of mode change.
     * 
     * @return The time of mode change.
     */
    public double getChangeTime() {
        return changeTime;
    }

    /**
     * Set the time of mode change.
     * 
     * @param changeTime The time of mode change.
     */
    public void setChangeTime(double changeTime) {
        this.changeTime = changeTime;
    }

    /**
     * Getter for the rate at which this queue will grow.
     * 
     * @return The rate at which this queue will grow.
     */
    public double getMaxGrowthRate() {
        return maxGrowthRate;
    }

    /**
     * Setter for the rate at which this queue will grow.
     * 
     * @param maxGrowthRate The rate at which this queue will grow.
     */
    public void setMaxGrowthRate(double maxGrowthRate) {
        this.maxGrowthRate = maxGrowthRate;
    }

    /**
     * Add to the growth of the queue.
     * 
     * @param growth The amount of queue growth to add.
     */
    public void grow(double growth) {
        projectedQueue += growth;
    }

    /**
     * Get the projected length of the queue.
     * 
     * @return The projected length of the queue.
     */
    public double getProjectedQueue() {
        return projectedQueue;
    }

    /**
     * Set the projected length of the queue.
     * 
     * @param projectedQueue The projected length of the queue.
     */
    void setProjectedQueue(double projectedQueue) {
        this.projectedQueue = projectedQueue;
    }

    /**
     * Contribute to the discharging queue.
     * 
     * @param discharge The amount of queue that has discharged.
     */
    public void discharge(double discharge) {
        projectedDischarge += discharge;
    }

    /**
     * Get the projected discharge position.
     * 
     * @return The projected discharge position.
     */
    public double getProjectedDischarge() {
        return projectedDischarge;
    }

    /**
     * Set the projected discharge position.
     * 
     * @param projectedDischarge The projected discharge position.
     */
    void setProjectedDischarge(double projectedDischarge) {
        this.projectedDischarge = projectedDischarge;
    }

    /**
     * Get the cumulative totals of queue lengths per cycle.
     * 
     * @return The cumulative totals of queue lengths per cycle.
     */
    public double getTotalTravelTime() {
        return totalTravelTime;
    }

    /**
     * Set the cumulative totals of queue lengths per cycle.
     * 
     * @param totalTravelTime The cumulative totals of queue lengths per cycle.
     */
    public void setTotalTravelTime(double totalTravelTime) {
        this.totalTravelTime = totalTravelTime;
    }

}
