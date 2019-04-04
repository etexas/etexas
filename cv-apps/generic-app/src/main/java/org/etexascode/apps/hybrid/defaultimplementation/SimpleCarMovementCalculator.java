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
package org.etexascode.apps.hybrid.defaultimplementation;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.apps.hybrid.interfaces.ICarFollowingCalculator;
import org.etexascode.apps.hybrid.interfaces.ICarMovementCalculator;
import org.etexascode.apps.hybrid.interfaces.ILaneChangeCalculator;
import org.etexascode.apps.hybrid.interfaces.ILeftTurnCalculator;
import org.etexascode.apps.hybrid.interfaces.IRightTurnCalculator;
import org.etexascode.apps.hybrid.interfaces.IVehiclesByLane;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.Vehicle;

/**
 * Basic implementation of the means of moving cars.
 * 
 * @author ablatt
 */
public class SimpleCarMovementCalculator implements ICarMovementCalculator {

    /**
     * function which gets the vehicles by the lane
     */
    IVehiclesByLane vehByLane = new SimpleVehiclesByLane();

    /**
     * function which deals with lane changes
     */
    ILaneChangeCalculator laneChanges = new SimpleLaneChangeCalculator();

    /**
     * function which deals with left turns
     */
    ILeftTurnCalculator leftTurns = new SimpleLeftTurnCalculator();

    /**
     * function which deals with right turns
     */
    IRightTurnCalculator rightTurns = new SimpleRightTurnCalculator();

    /**
     * function which deals with car following
     */
    ICarFollowingCalculator carFollowing = new SuperSimpleCarFollowingCalculator();

    @Override
    public List<Vehicle> getCarMovements(List<Vehicle> nonDsrc, InterRepInfoModel model) {
        Map<Integer, List<Vehicle>> tmp = vehByLane.filterVehiclesByLane(nonDsrc);
        tmp = laneChanges.getLaneChanges(tmp, model);
        tmp = leftTurns.getLeftTurns(tmp, model);
        tmp = rightTurns.getRightTurns(tmp, model);
        tmp = carFollowing.performCarFollowing(tmp, model);

        List<Vehicle> ret = new LinkedList<Vehicle>();

        for (Entry<Integer, List<Vehicle>> entry : tmp.entrySet()) {
            ret.addAll(entry.getValue());
        }

        return ret;
    }

    /**
     * Getter
     * 
     * @return function which gets the vehicles by the lane
     */
    public IVehiclesByLane getVehiclesByLaneCalculator() {
        return vehByLane;
    }

    /**
     * Setter
     * 
     * @param calc function which gets the vehicles by the lane
     */
    public void setVehiclesByLaneCalculator(IVehiclesByLane calc) {
        vehByLane = calc;
    }

    /**
     * Getter
     * 
     * @return function which deals with lane changes
     */
    public ILaneChangeCalculator getLaneChangeCalculator() {
        return laneChanges;
    }

    /**
     * Setter
     * 
     * @param calc function which deals with lane changes
     */
    public void setLaneChangeCalculator(ILaneChangeCalculator calc) {
        laneChanges = calc;
    }

    /**
     * Getter
     * 
     * @return function which deals with left turns
     */
    public ILeftTurnCalculator getLeftTurnCalculator() {
        return leftTurns;
    }

    /**
     * Setter
     * 
     * @param calc function which deals with left turns
     */
    public void setLeftTurnCalculator(ILeftTurnCalculator calc) {
        leftTurns = calc;
    }

    /**
     * Getter
     * 
     * @return function which deals with right turns
     */
    public IRightTurnCalculator getRightTurnCalculator() {
        return rightTurns;
    }

    /**
     * Setter
     * 
     * @param calc function which deals with right turns
     */
    public void setRightTurnCalculator(IRightTurnCalculator calc) {
        rightTurns = calc;
    }

    /**
     * Getter
     * 
     * @return function which deals with car following
     */
    public ICarFollowingCalculator getCarFollowingCalculator() {
        return carFollowing;
    }

    /**
     * Setter
     * 
     * @param calc function which deals with car following
     */
    public void setCarFollowingCalculator(ICarFollowingCalculator calc) {
        carFollowing = calc;
    }
}
